package com.jouwee.erlang;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JFrame;

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
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(new PanelSimulacao(model));
        getContentPane().add(new PanelControle(model), BorderLayout.SOUTH);
       
    }
    
}
