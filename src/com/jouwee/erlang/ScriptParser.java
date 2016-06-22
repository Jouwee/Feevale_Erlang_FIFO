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
    }
    
    /**
     * Interpreta uma nova entrada de log
     * 
     * @param line 
     */
    private void parseNewLogEntry(String line) {
        logEntry = line.substring(8).trim();
        parseLogEntry();
    }
    
    /**
     * Continua interpretando uma entrada de log
     */
    private void continueParsingEnty(String line) {
        logEntry += line.trim();
        parseLogEntry();
    }
    
    /**
     * Interpreta a entrada de log carregada
     */
    private void parseLogEntry() {
        System.out.println(logEntry);
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
        testUpdateList(content);
        testProdutorStarted(processName, pid, content);
        testStatusAgent(processName, content);
    }
    
    /**
     * Testa se um produtor foi criado
     * 
     * @param processName
     * @param pid
     * @param content 
     */
    private void testProdutorStarted(String processName, String pid, String content) {
        Matcher matcher = PRODUTOR_STARTED.matcher(content);
        if (!matcher.find()) {
            return;
        }
        model.putProdutor(new Produtor(processName, StatusAgente.WAITING));
    }
    
    /**
     * Testa se um produtor foi criado
     * 
     * @param processName
     * @param pid
     * @param content 
     */
    private void testStatusAgent(String processName, String content) {
        Matcher matcher = STATUS_AGENTE.matcher(content);
        if (!matcher.find()) {
            return;
        }
        model.putProdutor(new Produtor(processName, StatusAgente.forScript(matcher.group(1))));
    }
    
    /**
     * Testa a atualização da lista
     * 
     * @param content 
     */
    private void testUpdateList(String content) {
        Matcher matcher = PATTERN_UPDATE_LIST.matcher(content);
        if (!matcher.find()) {
            return;
        }
        Matcher elementosMatcher = PATTERN_SPLIT_LIST.matcher(matcher.group(1));
        List<ItemFila> fila = new ArrayList<>();
        while (elementosMatcher.find()) {
            fila.add(factoryItemFila(elementosMatcher));
        }
        model.setFila(fila.toArray(new ItemFila[] {}));
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
        String produtor = matcher.group(3);
        String consumidor = matcher.group(4);
        return new ItemFila(ItemStatus.forScript(status));
    }

}
