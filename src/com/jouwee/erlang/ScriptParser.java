package com.jouwee.erlang;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;

/**
 * Parser da saída do Script
 *
 * @author Nicolas
 */
public class ScriptParser implements ScriptPatterns {

    /** Model a ser atualizado com a saída do Script */
    private final ScriptModel model;
    /** Entrada do log */
    private String logEntry;

    /**
     * Cria um novo parser
     */
    public ScriptParser() {
        this(new ScriptModel());
    }

    /**
     * Cria um novo parser
     *
     * @param model
     */
    public ScriptParser(ScriptModel model) {
        this.model = model;
    }

    /**
     * Interpreta a saída de um InputStream em background
     * 
     * @param inputStream
     */
    public void parseInBackground(InputStream inputStream) {
        Thread threadParser = new Thread(() -> {
            parse(inputStream);
        });
        threadParser.setDaemon(true);
        threadParser.start();
    }
    
    /**
     * Interpreta a saída de um InputStream
     *
     * @param inputStream
     */
    public void parse(InputStream inputStream) {
        final BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
        try {
            while (true) {
                String line = reader.readLine();
                if (line == null) {
                    return;
                }
                parseLine(removeUnwantedText(line));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    /**
     * Remove texto desnecessário
     * 
     * @param line
     * @return String
     */
    public String removeUnwantedText(String line) {
        String clearLine = line;
        // Remove as linhas do Shell
        if (clearLine.startsWith("1> ")) {
            clearLine = clearLine.substring(3);
        }
        // Remove a linha de cabeçalho do Shell
        clearLine = clearLine.replace("Eshell V7.3  (abort with ^G)", "");
        return clearLine;
    }
    
    /**
     * Interpreta uma linha
     * 
     * @param line 
     */
    public void parseLine(String line) {
        // Se for uma nova entrada de log
        if (line.startsWith("#Logger")) {
            parseNewLogEntry(line);
        } else {
            continueParsingEnty(line);
        }
        if (line.endsWith("#End")) {
            parseLogEntry();
        }
    }
    
    /**
     * Interpreta uma nova entrada de log
     * 
     * @param line 
     */
    private void parseNewLogEntry(String line) {
        logEntry = line.substring(8).trim();
    }
    
    /**
     * Continua interpretando uma entrada de log
     */
    private void continueParsingEnty(String line) {
        logEntry += line.trim();
    }
    
    /**
     * Interpreta a entrada de log carregada
     */
    private void parseLogEntry() {
        Matcher matcherNamePid = PATTERN_NAME_PID.matcher(logEntry);
        if (!matcherNamePid.find()) {
            return;
        }
        testMatchers(matcherNamePid.group(1), matcherNamePid.group(2), matcherNamePid.group(3));
    }
    
    /**
     * Testa os diversos Matchers
     * 
     * @param processName
     * @param pid
     * @param content 
     */
    private void testMatchers(String processName, String pid, String content) {
        if (testProgressAgent(processName, content)) {
            return;
        }        
        if (testUpdateList(content)) {
            return;
        }
        if (testStatusAgent(processName, content)) {
            return;
        }
        if (testProdutorStarted(processName, pid, content)) {
            return;
        }
        if (testConsumidorStarted(processName, pid, content)) {
            return;
        }
    }
    
    /**
     * Testa se um produtor foi criado
     * 
     * @param processName
     * @param pid
     * @param content 
     */
    private boolean testProdutorStarted(String processName, String pid, String content) {
        Matcher matcher = PRODUTOR_STARTED.matcher(content);
        if (!matcher.find()) {
            return false;
        }
        model.putProdutor(new Produtor(processName, StatusAgente.WAITING, 0));
        return true;
    }
    
    /**
     * Testa se um produtor foi criado
     * 
     * @param processName
     * @param pid
     * @param content 
     */
    private boolean testConsumidorStarted(String processName, String pid, String content) {
        Matcher matcher = CONSUMIDOR_STARTED.matcher(content);
        if (!matcher.find()) {
            return false;
        }
        model.putConsumidor(new Consumidor(processName, StatusAgente.WAITING, 0));
        return true;
    }
    
    /**
     * Testa se um produtor foi criado
     * 
     * @param processName
     * @param pid
     * @param content 
     */
    private boolean testStatusAgent(String processName, String content) {
        Matcher matcher = STATUS_AGENTE.matcher(content);
        if (!matcher.find()) {
            return false;
        }
        Agente agente = model.getAgente(processName);
        if (processName.startsWith("Produtor")) {
            model.putProdutor(new Produtor(processName, StatusAgente.forScript(matcher.group(1)), agente.getPercentDone()));
        } else {
            model.putConsumidor(new Consumidor(processName, StatusAgente.forScript(matcher.group(1)), agente.getPercentDone()));
        }
        return true;
    }
    
    /**
     * Testa o progresso de um agente
     * 
     * @param processName
     * @param pid
     * @param content 
     */
    private boolean testProgressAgent(String processName, String content) {
        Matcher matcher = PROGRESS_AGENTE.matcher(content);
        if (!matcher.find()) {
            return false;
        }
        float progress = Float.parseFloat(matcher.group(1));
        Agente agente = model.getAgente(processName);
        if (processName.startsWith("Produtor")) {
            model.putProdutor(new Produtor(processName, agente.getStatus(), progress));
        } else {
            model.putConsumidor(new Consumidor(processName, agente.getStatus(), progress));
        }
        return true;
    }
    
    /**
     * Testa a atualização da lista
     * 
     * @param content 
     */
    private boolean testUpdateList(String content) {
        Matcher matcher = PATTERN_UPDATE_LIST.matcher(content);
        if (!matcher.find()) {
            return false;
        }
        Matcher elementosMatcher = PATTERN_SPLIT_LIST.matcher(matcher.group(1));
        List<ItemFila> fila = new ArrayList<>();
        while (elementosMatcher.find()) {
            fila.add(factoryItemFila(elementosMatcher));
        }
        model.setFila(fila.toArray(new ItemFila[] {}));
        return true;
    }
    
    /**
     * Cria o item da fila a partir do Matcher
     * 
     * @param matcher 
     * @return ItemFIla
     */
    private ItemFila factoryItemFila(Matcher matcher) {
        String status = matcher.group(1);
        String id = matcher.group(2);
        String produtor = matcher.group(3).replace("[]", "");
        String consumidor = matcher.group(4).replace("[]", "");
        return new ItemFila(ItemStatus.forScript(status), produtor, consumidor);
    }

}
