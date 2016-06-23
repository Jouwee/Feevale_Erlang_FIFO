package com.jouwee.erlang;

import java.awt.Color;

/**
 * Status de um item da fila
 * 
 * @author Nicolas
 */
public enum ItemStatus {
    
    EMPTY("empty", Color.GRAY, "Vazio"),
    BEING_PRODUCED("reserved", Color.YELLOW, "Produz."),
    PRODUCED("produced", Color.ORANGE, "Aguard."),
    BEING_CONSUMED("reservedForConsumption", Color.GREEN, "Consum.")
    ;
    
    /** String no Script */
    private final String scriptString;
    /** Cor */
    private final Color color;
    /** Descrição do status */
    private final String description;

    /**
     * Status do item
     * 
     * @param scriptString 
     * @param color
     */
    private ItemStatus(String scriptString, Color color, String description) {
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
     * Retorna a descrição do status
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
    public static ItemStatus forScript(String string) {
        for (ItemStatus value : values()) {
            if (value.getScriptString().equalsIgnoreCase(string)) {
                return value;
            }
        }
        return null;
    }
    
}
