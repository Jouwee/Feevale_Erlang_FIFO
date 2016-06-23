package com.jouwee.erlang;

import java.awt.Color;

/**
 * Status dos itens
 * 
 * @author Nicolas Pohren
 */
public enum StatusAgente {
    
    WAITING("waiting", Color.GRAY, "Ocioso"),
    PRODUCING("producing", Color.YELLOW, "Produzindo"),
    CONSUMING("consuming", Color.GREEN, "Consumindo")
    ;
    
    /** String no Script */
    private final String scriptString;
    /** Cor */
    private final Color color;
    /** Descrição */
    private final String description;

    /**
     * Status do item
     * 
     * @param scriptString 
     * @param color
     * @param description
     */
    private StatusAgente(String scriptString, Color color, String description) {
        this.scriptString = scriptString;
        this.color = color;
        this.description = description;
    }

    /**
     * Retorna a String do Script
     * 
     * @return String
     */
    public String getScriptString() {
        return scriptString;
    }

    /**
     * Retorna a cor do status
     * 
     * @return Color
     */
    public Color getColor() {
        return color;
    }

    /**
     * Retorna a descrição do agente
     * 
     * @return String
     */
    public String getDescription() {
        return description;
    }
    
    /**
     * Retorna o item a partir da String do script
     * 
     * @param string
     * @return ItemStatus
     */
    public static StatusAgente forScript(String string) {
        for (StatusAgente value : values()) {
            if (value.getScriptString().equalsIgnoreCase(string)) {
                return value;
            }
        }
        return null;
    }
    
}
