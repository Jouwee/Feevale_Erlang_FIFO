package com.jouwee.erlang;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.text.DecimalFormat;
import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.text.NumberFormatter;

/**
 * Painel de controle do programa
 *
 * @author Nicolas Pohren
 */
public class PanelControle extends JComponent {

    /** Modelo */
    private final ScriptModel model;
    /** Field para o número de produtores */
    private JFormattedTextField fNumeroProdutores;

    /**
     * Cria o painel de controle
     *
     * @param model
     */
    public PanelControle(ScriptModel model) {
        this.model = model;
        initGui();
    }

    /**
     * Inicializa a interface
     */
    private void initGui() {
        setLayout(new GridLayout(7, 0, 5, 5));
        add(new JLabel("Número de produtores", JLabel.RIGHT));
        add(buildFieldNumeroProdutores());
        add(new JButton(new ActionExecutar()));
    }

    /**
     * Cria o campo de número de produtores
     * 
     * @return JComponent
     */
    private JComponent buildFieldNumeroProdutores() {
        fNumeroProdutores = new JFormattedTextField(new NumberFormatter(new DecimalFormat("#0")));
        fNumeroProdutores.setValue(model.getParameters().getNumeroProdutores());
        return fNumeroProdutores;
    }

    private class ActionExecutar extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            model.getParameters().setNumeroProdutores(((Number)fNumeroProdutores.getValue()).intValue());
            ErlangRunner.run("dataServer.erl", model);
        }
    }

}
