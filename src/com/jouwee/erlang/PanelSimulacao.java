package com.jouwee.erlang;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;
import javax.swing.JComponent;

/**
 * Painel da simulação
 * 
 * @author Nicolas Pohren
 */
public class PanelSimulacao extends JComponent implements PropertyChangeListener {
    
    /** Modelo da saída do Script */
    private ScriptModel model;

    /**
     * Cria o painel de simulação
     */
    public PanelSimulacao() {
        this(null);
    }
    
    /**
     * Cria o painel da simulação
     * 
     * @param model 
     */
    public PanelSimulacao(ScriptModel model) {
        this.model = model;
        model.getPropertyChangeSupport().addPropertyChangeListener(this);
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        repaint();
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setColor(Color.WHITE);
        g2d.fill(g2d.getClipBounds());
        paintProdutores(g2d);
        paintFila(g2d);
        g2d.dispose();
    }

    /**
     * Desenha os produtores
     * 
     * @param g 
     */
    private void paintProdutores(Graphics g) {
        if (model.getFila() == null) {
            return;
        }
        Graphics2D g2d = (Graphics2D) g.create();
        List<Produtor> produtores = model.getProdutores();
        for (int i = 0; i < produtores.size(); i++) {
            paintProdutor(g2d, produtores.get(i), i);
        }
        g2d.dispose();
    }

    /**
     * Desenha um produtor
     * 
     * @param g 
     * @param produtor
     * @param i
     */
    private void paintProdutor(Graphics g, Produtor produtor, int i) {
        Graphics2D g2d = (Graphics2D) g.create();
        int size = 30;
        int x = 30 + (i * (size + 5));
        int y = 30;
        g2d.setColor(Color.RED);
        g2d.fillRect(x, y, size, size);
        g2d.dispose();
    }
    
    /**
     * Desenha a fila
     * 
     * @param g 
     */
    private void paintFila(Graphics g) {
        if (model.getFila() == null) {
            return;
        }
        Graphics2D g2d = (Graphics2D) g.create();
        ItemFila[] fila = model.getFila();
        for (int i = 0; i < fila.length; i++) {
            paintItemFila(g2d, fila[i], i);
        }
        g2d.dispose();
    }
    
    /**
     * Desenha a fila
     * 
     * @param g 
     * @param item
     * @param i
     */
    private void paintItemFila(Graphics g, ItemFila item, int i) {
        Graphics2D g2d = (Graphics2D) g.create();
        int size = 30;
        int x = 30 + (i * (size + 5));
        int y = 100;
        g2d.setColor(item.getStatus().getColor());
        g2d.fillRect(x, y, size, size);
        g2d.dispose();
    }
    
}
