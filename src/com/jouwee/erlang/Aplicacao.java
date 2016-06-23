package com.jouwee.erlang;

import java.awt.BorderLayout;
import javax.swing.JFrame;
import javax.swing.JScrollPane;

/**
 * Classe principal da aplicação
 * 
 * @author Nícolas Pohren
 */
public class Aplicacao extends JFrame {
    
    /** Modelo do Script */
    public final ScriptModel model;
    
    public static void main(String[] args) {
        Aplicacao app = new Aplicacao();
        app.setVisible(true);
    }

    /**
     * Cria a aplicação 
     */
    public Aplicacao() {
        super("Produtor/Consumidor Erlang");
        model = new ScriptModel();
        initGui();
    }

    /**
     * Inicializa a interface
     */
    private void initGui() {
        setSize(1024, 600);
        setResizable(false);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(new JScrollPane(new PanelSimulacao(model)));
        getContentPane().add(new PanelControle(model), BorderLayout.SOUTH);
       
    }
    
}
