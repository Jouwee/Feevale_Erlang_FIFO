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
    /** Parâmetros do Script */
    private final ScriptParameters parameters;
    /** Itens da fila */
    private ItemFila[] fila;
    /** Produtores */
    private final Map<String, Produtor> produtores;
    /** Consumidores */
    private final Map<String, Consumidor> consumidores;

    /**
     * Cria novo modelo
     */
    public ScriptModel() {
        this.propertyChangeSupport = new PropertyChangeSupport(this);
        this.produtores = new HashMap<>();
        this.consumidores = new HashMap<>();
        this.parameters = new ScriptParameters();
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
        propertyChangeSupport.firePropertyChange("produtores", null, produtores);
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
     * Adiciona consumidor na lista
     * 
     * @param consumidor 
     */
    public void putConsumidor(Consumidor consumidor) {
        consumidores.put(consumidor.getName(), consumidor);
        propertyChangeSupport.firePropertyChange("consumidores", null, consumidores);
    }
    
    /**
     * Retorna os consumidores
     * 
     * @return {@code Collection<Consumidor>}
     */
    public List<Consumidor> getConsumidores() {
        return new ArrayList<>(consumidores.values());
    }

    /**
     * Retorna o propertyChangeSupport
     * 
     * @return PropertyChangeSupport
     */
    public PropertyChangeSupport getPropertyChangeSupport() {
        return propertyChangeSupport;
    }

    /**
     * Retorna os parâmetros do Script
     * 
     * @return int
     */
    public ScriptParameters getParameters() {
        return parameters;
    }

    /**
     * Retorna um agente
     * 
     * @param processName
     * @return Agente
     */
    public Agente getAgente(String processName) {
        if (produtores.containsKey(processName)) {
            return produtores.get(processName);
        }
        return consumidores.get(processName);
    }
    
}
