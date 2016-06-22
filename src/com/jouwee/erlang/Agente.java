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
    private StatusAgente status;

    /**
     * Cria novo agente
     * 
     * @param name 
     * @param status 
     */
    public Agente(String name, StatusAgente status) {
        this.name = name;
        this.status = status;
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
    
}
