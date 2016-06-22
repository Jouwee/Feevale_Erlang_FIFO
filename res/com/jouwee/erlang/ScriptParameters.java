package com.jouwee.erlang;

/**
 * Parâmetros para chamada do Script
 * 
 * @author Nicolas Pohren
 */
class ScriptParameters {
    
    /** Número de produtores */
    private int numeroProdutores;

    /**
     * Cria os parâmetros para os scripts
     */
    public ScriptParameters() {
        numeroProdutores = 3;
    }
    
    /**
     * Retorna o número de produtores
     * 
     * @return int
     */
    public int getNumeroProdutores() {
        return numeroProdutores;
    }

    /**
     * Define o número de produtores
     * 
     * @param numeroProdutores 
     */
    public void setNumeroProdutores(int numeroProdutores) {
        this.numeroProdutores = numeroProdutores;
    }
    
}
