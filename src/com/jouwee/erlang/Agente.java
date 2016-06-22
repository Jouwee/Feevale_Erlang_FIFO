package com.jouwee.erlang;

import java.awt.Color;

/**
 * Agente
 * 
 * @author Nicolas Pohren
 */
public abstract class Agente {
    
    /** Nome do agente */
    private final String name;
    /** Status */
    private final StatusAgente status;
    /** Percentual feito */
    private final float percentDone;

    /**
     * Cria novo agente
     * 
     * @param name 
     * @param status 
     * @param percentDone 
     */
    public Agente(String name, StatusAgente status, float percentDone) {
        this.name = name;
        this.status = status;
        this.percentDone = percentDone;
    }
    
    /**
     * Retorna a cor para representar o agente
     * 
     * @return Color
     */
    public Color getColor() {
        return status.getColor();
    }
    
    /**
     * Retorna o nome 
     * 
     * @return String
     */
    public String getName() {
        return name;
    }

    /**
     * Retorna o status
     * 
     * @return StatusProdutor
     */
    public StatusAgente getStatus() {
        return status;
    }

    /**
     * Retorna o percentual feito
     * 
     * @return float
     */
    public float getPercentDone() {
        return percentDone;
    }
    
}
