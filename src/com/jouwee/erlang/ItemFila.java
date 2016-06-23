package com.jouwee.erlang;

/**
 * Item da fila 
 * 
 * @author Nicolas Pohren
 */
public class ItemFila {
    
    /** Status */
    private final ItemStatus status;
    /** Nome do produtor */
    private final String produtor;
    /** Nome do consumidor */
    private final String consumidor;

    /**
     * Cria um novo item da fila
     * 
     * @param status 
     * @param produtor 
     * @param consumidor 
     */
    public ItemFila(ItemStatus status, String produtor, String consumidor) {
        this.status = status;
        this.produtor = produtor;
        this.consumidor = consumidor;
    }

    /**
     * Retorna o status
     * 
     * @return ItemStatus
     */
    public ItemStatus getStatus() {
        return status;
    }

    /**
     * Retorna o nome do produtor
     * 
     * @return String
     */
    public String getProdutor() {
        return produtor;
    }

    /**
     * Retorna o nome do consumidor
     * 
     * @return String
     */
    public String getConsumidor() {
        return consumidor;
    }
    
}
