package com.jouwee.erlang;

/**
 * Parâmetros para chamada do Script
 * 
 * @author Nicolas Pohren
 */
class ScriptParameters {
    
    /** Número de produtores */
    private int tamanhoFila;
    /** Número de produtores */
    private int numeroProdutores;
    /** Tempo médio de produção */
    private int tempoMedioProducao;
    /** Desvio padrão do tempo de produção */
    private int desvioPadraoTempoProducao;
    /** Número de consumidores */
    private int numeroConsumidores;
    /** Tempo médio de Consumo */
    private int tempoMedioConsumo;
    /** Desvio padrão do tempo de Consumo */
    private int desvioPadraoTempoConsumo;
    /** Tempo de espera entre verificações de tarefas */
    private int tempoEspera;

    /**
     * Cria os parâmetros para os scripts
     */
    public ScriptParameters() {
        tamanhoFila = 10;
        numeroProdutores = 3;
        tempoMedioProducao = 1000;
        desvioPadraoTempoProducao = 100;
        numeroConsumidores = 3;
        tempoMedioConsumo = 1000;
        desvioPadraoTempoConsumo = 100;
        tempoEspera = 100;
    }

    /**
     * Retorna o tamanho da fila
     * 
     * @return int
     */
    public int getTamanhoFila() {
        return tamanhoFila;
    }

    /**
     * Define o tamanho da fila
     * 
     * @param tamanhoFila 
     */
    public void setTamanhoFila(int tamanhoFila) {
        this.tamanhoFila = tamanhoFila;
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

    /**
     * Retorna o tempo médio de produção
     * 
     * @return int
     */
    public int getTempoMedioProducao() {
        return tempoMedioProducao;
    }

    /**
     * Define o tempo médio de produção
     * 
     * @param tempoMedioProducao 
     */
    public void setTempoMedioProducao(int tempoMedioProducao) {
        this.tempoMedioProducao = tempoMedioProducao;
    }

    /**
     * Retorna o desvio padrão do tempo de produção
     * 
     * @return int
     */
    public int getDesvioPadraoTempoProducao() {
        return desvioPadraoTempoProducao;
    }

    /**
     * Define o desvio padrão do tempo de produção
     * 
     * @param desvioPadraoTempoProducao 
     */
    public void setDesvioPadraoTempoProducao(int desvioPadraoTempoProducao) {
        this.desvioPadraoTempoProducao = desvioPadraoTempoProducao;
    }

    /**
     * Retorna o número de consumidores
     * 
     * @return int
     */
    public int getNumeroConsumidores() {
        return numeroConsumidores;
    }

    /**
     * Define o número de consumidores
     * 
     * @param numeroConsumidores 
     */
    public void setNumeroConsumidores(int numeroConsumidores) {
        this.numeroConsumidores = numeroConsumidores;
    }

    /**
     * Retorna o tempo médio de consumo
     * 
     * @return int
     */
    public int getTempoMedioConsumo() {
        return tempoMedioConsumo;
    }

    /**
     * Define o tempo médio de consumo
     * 
     * @param tempoMedioConsumo 
     */
    public void setTempoMedioConsumo(int tempoMedioConsumo) {
        this.tempoMedioConsumo = tempoMedioConsumo;
    }

    /**
     * Retorna o desvio padrão do tempo de consumo
     * 
     * @return int
     */
    public int getDesvioPadraoTempoConsumo() {
        return desvioPadraoTempoConsumo;
    }

    /**
     * Define o desvio padrão do tempo de consumo
     * 
     * @param desvioPadraoTempoConsumo 
     */
    public void setDesvioPadraoTempoConsumo(int desvioPadraoTempoConsumo) {
        this.desvioPadraoTempoConsumo = desvioPadraoTempoConsumo;
    }

    /**
     * Retorna o tempo de espera entre verificações
     * 
     * @return int
     */
    public int getTempoEspera() {
        return tempoEspera;
    }

    /**
     * Define o tempo de espera entre verificações
     * 
     * @param tempoEspera 
     */
    public void setTempoEspera(int tempoEspera) {
        this.tempoEspera = tempoEspera;
    }
    
}
