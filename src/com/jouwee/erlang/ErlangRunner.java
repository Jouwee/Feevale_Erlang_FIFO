package com.jouwee.erlang;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

/**
 *
 *
 * @author Nicolas Pohren
 */
public class ErlangRunner {

    private static final String COMPILADOR = "c:\\Program Files\\erl7.3\\bin\\erlc.exe";
    private static final String EXECUTAVEL = "c:\\Program Files\\erl7.3\\bin\\erl.exe";

    public static void run(String resourceName, ScriptModel model) {
        try {
            String fileName = copyStream(resourceName);
            ProcessBuilder builderCompilador = new ProcessBuilder(COMPILADOR, resourceName);
            builderCompilador.directory(new File(System.getProperty("java.io.tmpdir")));
            builderCompilador.inheritIO();
            Process compilacao = builderCompilador.start();
            System.out.println("compilando");
            compilacao.waitFor();
            System.out.println("compilou");
            // Converte os parâmetros para String
            String sTamanhoFila = String.valueOf(model.getParameters().getTamanhoFila());
            String sNumeroProdutores = String.valueOf(model.getParameters().getNumeroProdutores());
            String sTempoMedioProducao = String.valueOf(model.getParameters().getTempoMedioProducao());
            String sDesvioPadraoTempoProducao = String.valueOf(model.getParameters().getDesvioPadraoTempoProducao());
            String sNumeroConsumidores = String.valueOf(model.getParameters().getNumeroConsumidores());
            String sTempoMedioConsumo = String.valueOf(model.getParameters().getTempoMedioConsumo());
            String sDesvioPadraoTempoConsumo = String.valueOf(model.getParameters().getDesvioPadraoTempoConsumo());
            String sTempoEspera = String.valueOf(model.getParameters().getTempoEspera());
            // Executa o Script Erlang
            ProcessBuilder builder = new ProcessBuilder(EXECUTAVEL, "-noshell", "-run", "dataServer", "main", sTamanhoFila, sNumeroProdutores, sNumeroConsumidores, sTempoMedioProducao, sDesvioPadraoTempoProducao, sTempoMedioConsumo, sDesvioPadraoTempoConsumo, sTempoEspera);
            builder.directory(new File(System.getProperty("java.io.tmpdir")));
            Process process = builder.start();
            new ScriptParser(model).parseInBackground(process.getInputStream());

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static String copyStream(String resourceName) throws IOException {
        String name = System.getProperty("java.io.tmpdir") + resourceName;
        InputStream stream = ErlangRunner.class.getResourceAsStream(resourceName);
        OutputStream os = new FileOutputStream(name);
        while (true) {
            int byt = stream.read();
            if (byt == -1) {
                break;
            }
            os.write(byt);
        }
        stream.close();
        os.close();
        return name;
    }

}
