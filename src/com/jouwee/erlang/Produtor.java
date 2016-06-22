package com.jouwee.erlang;

import java.awt.Color;

/**
 * Produtor
 * 
 * @author Nicolas Pohren
 */
public class Produtor extends Agente {
    
    /**
     * Cria novo produtor
     * 
     * @param name 
     * @param status 
     * @param percentDone 
     */
    public Produtor(String name, StatusAgente status, float percentDone) {
        super(name, status, percentDone);
    }
    
}
