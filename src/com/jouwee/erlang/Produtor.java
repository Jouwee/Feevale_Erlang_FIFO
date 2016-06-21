package com.jouwee.erlang;

/**
 * Produtor
 * 
 * @author Nicolas Pohren
 */
public class Produtor {
    
    /** Nome do produtor */
    private final String name;

    /**
     * Cria novo produtor
     * @param name 
     */
    public Produtor(String name) {
        this.name = name;
    }
    
    /**
     * Retorna o nome do produtor
     * 
     * @return String
     */
    public String getName() {
        return name;
    }
    
}
