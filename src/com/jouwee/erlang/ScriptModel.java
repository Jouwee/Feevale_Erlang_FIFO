package com.jouwee.erlang;

import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Modelo da saída da execução do Script
 * 
 * @author Nícolas Pohren
 */
public class ScriptModel {
    
    /** Suporte a PropertyChange */
    public final PropertyChangeSupport propertyChangeSupport;
    /** Itens da fila */
    private ItemFila[] fila;
    /** Produtores */
    private Map<String, Produtor> produtores;

    /**
     * Cria novo modelo
     */
    public ScriptModel() {
        this.propertyChangeSupport = new PropertyChangeSupport(this);
        this.produtores = new HashMap<>();
    }

    /**
     * Retorna a fila
     * 
     * @return ItemFila[]
     */
    public ItemFila[] getFila() {
        return fila;
    }

    /**
     * Define a fila
     * 
     * @param fila 
     */
    public void setFila(ItemFila[] fila) {
        this.fila = fila;
        propertyChangeSupport.firePropertyChange("fila", null, fila);
    }
    
    /**
     * Adiciona produtor na lista
     * 
     * @param produtor 
     */
    public void putProdutor(Produtor produtor) {
        produtores.put(produtor.getName(), produtor);
        propertyChangeSupport.firePropertyChange("produtores", null, fila);
    }
    
    /**
     * Retorna os produtores
     * 
     * @return {@code Collection<Produtor>}
     */
    public List<Produtor> getProdutores() {
        return new ArrayList<>(produtores.values());
    }

    /**
     * Retorna o propertyChangeSupport
     * 
     * @return PropertyChangeSupport
     */
    public PropertyChangeSupport getPropertyChangeSupport() {
        return propertyChangeSupport;
    }
    
}
