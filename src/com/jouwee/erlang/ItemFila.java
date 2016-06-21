package com.jouwee.erlang;

/**
 * Item da fila 
 * 
 * @author Nicolas Pohren
 */
public class ItemFila {
    
    /** Status */
    private final ItemStatus status;

    /**
     * Cria um novo item da fila
     * 
     * @param status 
     */
    public ItemFila(ItemStatus status) {
        this.status = status;
    }

    /**
     * Retorna o status
     * 
     * @return ItemStatus
     */
    public ItemStatus getStatus() {
        return status;
    }
    
}
