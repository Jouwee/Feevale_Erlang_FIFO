package com.jouwee.erlang;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
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

    /** Tamanho dos objetos de agentes */
    private static final Dimension SIZE_AGENT = new Dimension(125, 60);
    /** Tamanho dos objetos de item da fila */
    private static final int SIZE_ITEM_FILA = 60;
    /** Arco */
    private static final int ARC = 10;
    /** Modelo da saída do Script */
    private ScriptModel model;

    /**
     * Cria o painel de simulação
     */
    public PanelSimulacao() {
        this(null);
    }
    
    @Override
    public Dimension getPreferredSize() {
        int height = getHeight() - 20;
        int width = 100;
        int wt;
        wt = (model.getProdutores().size()) * (SIZE_AGENT.width + 5) + 60;
        if (wt > width) {
            width = wt;
        }
        wt = (model.getConsumidores().size()) * (SIZE_AGENT.width + 5) + 60;
        if (wt > width) {
            width = wt;
        }
        if (model.getFila() != null) {
            wt = ((model.getFila().length) / 4) * (SIZE_ITEM_FILA + 5) + 60;
            if (wt > width) {
                width = wt;
            }
        }
        return new Dimension(width, height);

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
        revalidate();
    }

    @Override
    protected void paintComponent(Graphics g) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        g2d.setColor(Color.WHITE);
        g2d.fill(g2d.getClipBounds());
        paintProdutores(g2d);
        paintFila(g2d);
        paintConsumidores(g2d);
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
        int x = 30 + (i * (SIZE_AGENT.width + 5));
        int y = 30;
        paintAgent(g, produtor, x, y);
        g2d.dispose();
    }
    
    /**
     * Desenha os consumidores
     * 
     * @param g 
     */
    private void paintConsumidores(Graphics g) {
        if (model.getFila() == null) {
            return;
        }
        Graphics2D g2d = (Graphics2D) g.create();
        List<Consumidor> consumidores = model.getConsumidores();
        for (int i = 0; i < consumidores.size(); i++) {
            paintConsumidor(g2d, consumidores.get(i), i);
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
    private void paintConsumidor(Graphics g, Consumidor produtor, int i) {
        Graphics2D g2d = (Graphics2D) g.create();
        int x = 30 + (i * (SIZE_AGENT.width + 5));
        int y = 355;
        paintAgent(g, produtor, x, y);
        g2d.dispose();
    }
    
    /**
     * Desenha o agente 
     * 
     * @param g
     * @param produtor 
     * @param x
     * @param y
     */
    private void paintAgent(Graphics g, Agente agente, int x, int y) {
        Graphics2D g2d = (Graphics2D) g.create();
        g2d.setColor(agente.getColor());
        g2d.fillRoundRect(x, y, SIZE_AGENT.width, SIZE_AGENT.height, ARC, ARC);
        g2d.setColor(Color.BLACK);
        g2d.drawRoundRect(x, y, SIZE_AGENT.width, SIZE_AGENT.height, ARC, ARC);
        g2d.setFont(new Font("Calibri", Font.BOLD, 16));
        g2d.drawString(agente.getName(), x + 5, y + 20);
        g2d.setFont(new Font("Calibri", Font.PLAIN, 11));
        g2d.drawString(agente.getStatus().getDescription(), x + 5, y + 35);
        g2d.fillRect(x, y + 50, (int)(SIZE_AGENT.width * agente.getPercentDone()), 5);
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
        int x = 30 + ((i/4) * (SIZE_ITEM_FILA + 5));
        int y = 95 + ((i%4) * (SIZE_ITEM_FILA + 5));
        g2d.setColor(item.getStatus().getColor());
        g2d.fillRoundRect(x, y, SIZE_ITEM_FILA, SIZE_AGENT.height, ARC, ARC);
        g2d.setColor(Color.BLACK);
        g2d.drawRoundRect(x, y, SIZE_ITEM_FILA, SIZE_AGENT.height, ARC, ARC);
        g2d.setFont(new Font("Calibri", Font.BOLD, 16));
        g2d.drawString(String.valueOf(i), x + 5, y + 20);
        if (!item.getConsumidor().isEmpty()) {
            String prod = "C"+item.getProdutor().substring(8);
            g2d.drawString(prod, x + 5, y + 35);
        } else {
            if (!item.getProdutor().isEmpty()) {
                String prod = "P"+item.getProdutor().substring(8);
                g2d.drawString(prod, x + 5, y + 35);
            }
        }
        g2d.setFont(new Font("Calibri", Font.PLAIN, 11));
        g2d.drawString(item.getStatus().getDescription(), x + 5, y + 50);
        g2d.dispose();
    }
    
}
