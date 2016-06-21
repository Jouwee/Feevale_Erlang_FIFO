package com.jouwee.erlang;

import java.util.regex.Pattern;

/**
 * Padrões de expressões regulares para as saídas do Script
 * 
 * @author Nicolas Pohren
 */
public interface ScriptPatterns {
    
    /** Regular Expression para obter o nome e o PID do processo */
    public static final Pattern PATTERN_NAME_PID = Pattern.compile("(.*?) (<.*>) (.*)");
    /** Regular Expression para obter a atualização da lista */
    public static final Pattern PATTERN_UPDATE_LIST = Pattern.compile("\\{updateFila,\\[(\\{.*?\\})\\]\\}");
    /** Regular Expression para quebrar a lista */
    public static final Pattern PATTERN_SPLIT_LIST = Pattern.compile("(?:\\{(.*?),(.*?),(.*?),(.*?)\\})");
    /** Produtor iniciado */
    public static final Pattern PRODUTOR_STARTED = Pattern.compile("produtorStarted");
    
}
