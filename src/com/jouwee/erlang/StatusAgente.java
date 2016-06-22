package com.jouwee.erlang;

import java.awt.Color;

/**
 * Status dos itens
 * 
 * @author Nicolas Pohren
 */
public enum StatusAgente {
    
    WAITING("waiting", Color.GRAY),
    PRODUCING("producing", Color.YELLOW),
    CONSUMING("consuming", Color.GREEN)
    ;
    
    /** String no Script */
    private final String scriptString;
    /** Cor */
    private final Color color;

    /**
     * Status do item
     * 
     * @param scriptString 
     * @param color
     */
    private StatusAgente(String scriptString, Color color) {
        this.scriptString = scriptString;
        this.color = color;
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
