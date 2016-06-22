package com.jouwee.erlang;

/**
 * Consumidor
 * 
 * @author Nicolas Pohren
 */
public class Consumidor extends Agente {
    
    /**
     * Cria novo produtor
     * 
     * @param name 
     * @param status 
     * @param percentDone 
     */
    public Consumidor(String name, StatusAgente status, float percentDone) {
        super(name, status, percentDone);
    }
   
}
