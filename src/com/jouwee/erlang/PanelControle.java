package com.jouwee.erlang;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.text.DecimalFormat;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
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
    /** Field para o tamanho da fila */
    private JFormattedTextField fTamanhoFila;
    /** Field para o número de produtores */
    private JFormattedTextField fNumeroProdutores;
    /** Field para o tempo médio de produção */
    private JFormattedTextField fTempoMedioProducao;
    /** Field para o desvio padrão do tempo de produção */
    private JFormattedTextField fDesvioPadraoProducao;
    /** Field para o número de consumidores */
    private JFormattedTextField fNumeroConsumidores;
    /** Field para o tempo médio de consumo */
    private JFormattedTextField fTempoMedioConsumo;
    /** Field para o desvio padrão do tempo de consumo */
    private JFormattedTextField fDesvioPadraoConsumo;
    /** Field para o tempo de espera */
    private JFormattedTextField fTempoEspera;

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
        setLayout(new GridLayout(0, 6, 5, 5));
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        add(new JLabel("Número de produtores", JLabel.RIGHT));
        add(buildFieldNumeroProdutores());
        add(new JLabel("Tempo médio de produção", JLabel.RIGHT));
        add(buildFieldTempoMedioProducao());
        add(new JLabel("Desvio padrão de produção", JLabel.RIGHT));
        add(buildFieldDesvioPadraoProducao());
        add(new JLabel("Número de consumidores", JLabel.RIGHT));
        add(buildFieldNumeroConsumidores());
        add(new JLabel("Tempo médio de consumo", JLabel.RIGHT));
        add(buildFieldTempoMedioConsumo());
        add(new JLabel("Desvio padrão de consumo", JLabel.RIGHT));
        add(buildFieldDesvioPadraoConsumo());
        add(new JLabel("Tamanho da fila", JLabel.RIGHT));
        add(buildFieldTamanhoFila());
        add(new JLabel("Tempo de espera", JLabel.RIGHT));
        add(buildFieldTempoEspera());
        add(new JButton(new ActionExecutar()));
    }

    /**
     * Cria o campo de tamanho da fila
     * 
     * @return JComponent
     */
    private JComponent buildFieldTamanhoFila() {
        fTamanhoFila = new JFormattedTextField(new NumberFormatter(new DecimalFormat("#0")));
        fTamanhoFila.setValue(model.getParameters().getTamanhoFila());
        return fTamanhoFila;
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

    /**
     * Cria o campo de tempo médio de produção
     * 
     * @return JComponent
     */
    private JComponent buildFieldTempoMedioProducao() {
        fTempoMedioProducao = new JFormattedTextField(new NumberFormatter(new DecimalFormat("#0")));
        fTempoMedioProducao.setValue(model.getParameters().getTempoMedioProducao());
        return fTempoMedioProducao;
    }

    /**
     * Cria o campo de desvio padrão do tempo de produção
     * 
     * @return JComponent
     */
    private JComponent buildFieldDesvioPadraoProducao() {
        fDesvioPadraoProducao = new JFormattedTextField(new NumberFormatter(new DecimalFormat("#0")));
        fDesvioPadraoProducao.setValue(model.getParameters().getDesvioPadraoTempoProducao());
        return fDesvioPadraoProducao;
    }

    /**
     * Cria o campo de número de consumidores
     * 
     * @return JComponent
     */
    private JComponent buildFieldNumeroConsumidores() {
        fNumeroConsumidores = new JFormattedTextField(new NumberFormatter(new DecimalFormat("#0")));
        fNumeroConsumidores.setValue(model.getParameters().getNumeroConsumidores());
        return fNumeroConsumidores;
    }

    /**
     * Cria o campo de tempo médio de consumo
     * 
     * @return JComponent
     */
    private JComponent buildFieldTempoMedioConsumo() {
        fTempoMedioConsumo = new JFormattedTextField(new NumberFormatter(new DecimalFormat("#0")));
        fTempoMedioConsumo.setValue(model.getParameters().getTempoMedioConsumo());
        return fTempoMedioConsumo;
    }

    /**
     * Cria o campo de número de produtores
     * 
     * @return JComponent
     */
    private JComponent buildFieldDesvioPadraoConsumo() {
        fDesvioPadraoConsumo = new JFormattedTextField(new NumberFormatter(new DecimalFormat("#0")));
        fDesvioPadraoConsumo.setValue(model.getParameters().getDesvioPadraoTempoConsumo());
        return fDesvioPadraoConsumo;
    }

    /**
     * Cria o campo de tempo de espera
     * 
     * @return JComponent
     */
    private JComponent buildFieldTempoEspera() {
        fTempoEspera = new JFormattedTextField(new NumberFormatter(new DecimalFormat("#0")));
        fTempoEspera.setValue(model.getParameters().getTempoEspera());
        return fTempoEspera;
    }

    private class ActionExecutar extends AbstractAction {

        public ActionExecutar() {
            super("Executar");
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
            model.getParameters().setTamanhoFila(((Number)fTamanhoFila.getValue()).intValue());
            model.getParameters().setNumeroProdutores(((Number)fNumeroProdutores.getValue()).intValue());
            model.getParameters().setTempoMedioProducao(((Number)fTempoMedioProducao.getValue()).intValue());
            model.getParameters().setDesvioPadraoTempoProducao(((Number)fDesvioPadraoProducao.getValue()).intValue());
            model.getParameters().setNumeroConsumidores(((Number)fNumeroConsumidores.getValue()).intValue());
            model.getParameters().setTempoMedioConsumo(((Number)fTempoMedioConsumo.getValue()).intValue());
            model.getParameters().setDesvioPadraoTempoConsumo(((Number)fDesvioPadraoConsumo.getValue()).intValue());
            model.getParameters().setTempoEspera(((Number)fTempoEspera.getValue()).intValue());
            ErlangRunner.run("dataServer.erl", model);
        }
    }

}
