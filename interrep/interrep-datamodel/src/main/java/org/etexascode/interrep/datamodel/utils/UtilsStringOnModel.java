/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */

package org.etexascode.interrep.datamodel.utils;

import java.awt.Polygon;
import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.etexascode.interrep.datamodel.interfaces.IDistanceable;

/**
 * A utility class for converting various types into strings in a uniform way. This class was
 * originally envisioned to help the toString methods in the datamodel, but could easily be used
 * elsewhere in the code.
 * 
 * @author ablatt
 * @author bmauldon
 */
public class UtilsStringOnModel {

    public final static Charset UTF8_CHARSET = Charset.forName("UTF-8");

    /**
     * Add a double to a string builder while giving that double a title (name) in a uniform way.
     * 
     * @param addTo The string builder to add the uniform string to.
     * @param toAdd The double to add.
     * @param title The title of the double to add.
     */
    public static void addDouble(StringBuilder addTo, double toAdd, String title) {
        addTo.append(title);
        addTo.append(" = ");
        addTo.append(toAdd);
        addTo.append("\n");
    }

    /**
     * Add an int to a string builder while giving that int a title (name) in a uniform way.
     * 
     * @param addTo The string builder to add the uniform string to.
     * @param toAdd The integer to add.
     * @param title The title of the integer to add.
     */
    public static void addInt(StringBuilder addTo, int toAdd, String title) {
        addTo.append(title);
        addTo.append(" = ");
        addTo.append(toAdd);
        addTo.append("\n");
    }

    /**
     * Add a list to a string builder while giving that list a title (name) in a uniform way.
     * 
     * @param <T> The type.
     * @param addTo The string builder to add the uniform string to.
     * @param toAdd The list to add.
     * @param listTitle The list title.
     */
    public static <T> void addList(StringBuilder addTo, List<T> toAdd, String listTitle) {
        for (T elem : toAdd) {
            addListElement(addTo, elem, listTitle);
        }
    }

    /**
     * Adds a set to a string builder while giving that set a title (name) in a uniform way.
     * 
     * @param <T> The type.
     * @param addTo The string builder to add the uniform string to.
     * @param toAdd The list to add.
     * @param listTitle The list title.
     */
    public static <T> void addSet(StringBuilder addTo, Set<T> toAdd, String listTitle) {
        Iterator<T> itr = toAdd.iterator();
        while (itr.hasNext()) {
            addListElement(addTo, itr.next(), listTitle);
        }
    }

    /**
     * Add a list element to a string builder while giving that list element a title (name) in a
     * uniform way.
     * 
     * @param <T> The type.
     * @param addTo The string builder to add the uniform string to.
     * @param toAdd The list to add.
     * @param listTitle The list title.
     */
    public static <T> void addListElement(StringBuilder addTo, T toAdd, String listTitle) {
        addTo.append(listTitle);
        addTo.append(" Element = ");
        addTo.append(toAdd.toString());
        addTo.append("\n");
    }

    /**
     * Add a map to a string builder while giving that map a title (name) in a uniform way.
     * 
     * @param <S> The key type.
     * @param <E> The value type.
     * @param addTo The string builder to add the uniform string to.
     * @param value The map to add.
     * @param mapTitle The map title.
     */
    public static <S, E> void addMap(StringBuilder addTo, Map<S, E> value, String mapTitle) {
        for (Entry<S, E> entry : value.entrySet()) {
            addMapKeyValue(addTo, entry.getKey(), entry.getValue(), mapTitle);
        }
    }

    /**
     * Add a map's key value pair to a string builder while giving that pair a title (name) in a
     * uniform way.
     * 
     * @param <E> The key type.
     * @param <T> The value type.
     * @param addTo The string builder to add the uniform string to.
     * @param key The key to add.
     * @param value The value to add.
     * @param mapTitle The map title.
     */
    public static <E, T> void addMapKeyValue(StringBuilder addTo, E key, T value, String mapTitle) {
        addTo.append(mapTitle);
        addTo.append(" Map: key = ");
        addTo.append(key.toString());
        addTo.append("\n     value = ");
        addTo.append(value.toString());
        addTo.append("\n");
    }

    /**
     * Add a string to a string builder while giving that string a title (name) in a uniform way.
     * 
     * @param addTo The string builder to add the uniform string to.
     * @param toAdd The string to add.
     * @param title The title of the string to add.
     */
    public static void addString(StringBuilder addTo, String toAdd, String title) {
        addTo.append(title);
        addTo.append(" = ");
        addTo.append(toAdd);
        addTo.append("\n");
    }

    /**
     * Build a string out of a point (x, y pair).
     * 
     * @param x The x coordinate of the point.
     * @param y The y coordinate of the point.
     * @return String The point string.
     */
    public static String buildPointString(int x, int y) {
        StringBuilder ret = new StringBuilder("(");

        ret.append(x);
        ret.append(", ");
        ret.append(y);
        ret.append(")\n");

        return ret.toString();
    }

    /**
     * Build a string out of a polygon.
     * 
     * @param p The polygon.
     * @return String The polygon string
     */
    public static String buildPolygonString(Polygon p) {
        StringBuilder ret = new StringBuilder();

        for (int i = 0; i < p.npoints; i++) {
            ret.append(buildPointString(p.xpoints[i], p.ypoints[i]));
        }

        return ret.toString();
    }

    /**
     * Decode a UTF8 byte buffer into a string.
     * 
     * @param bytes The UTF8 byte buffer.
     * @return String The decoded byte buffer.
     */
    public static String decodeUTF8(byte[] bytes) {
        return new String(bytes, UTF8_CHARSET);
    }

    /**
     * Converts a map (toConvert) into a string labeled by the title (title).
     * 
     * @param <E> The key type.
     * @param <F> The value type.
     * @param toConvert The map to convert into a string.
     * @param title The title of the map.
     * @return The converted map.
     */
    public static <E, F> String addMap(Map<E, F> toConvert, String title) {
        StringBuilder ret = new StringBuilder();

        ret.append("Map: ");
        ret.append(title);
        ret.append("\n");

        for (Entry<E, F> entry : toConvert.entrySet()) {
            ret.append(entry.getKey().toString());
            ret.append(": ");
            ret.append(entry.getValue().toString());
            ret.append("\n");
        }

        return ret.toString();
    }

    /**
     * Converts a list to a human readable string.
     * 
     * @param <E> The type.
     * @param toConvert The list to convert into a string.
     * @param title The title of the list.
     * @return The string form of list.
     */
    public static <E> String listToString(List<E> toConvert, String title) {
        StringBuilder ret = new StringBuilder();

        ret.append("List: ");
        ret.append(title);
        ret.append("\n");

        for (E item : toConvert) {
            ret.append("Entry: ");
            ret.append(item.toString());
            ret.append("\n");
        }

        return ret.toString();
    }

    /**
     * Converts an IDistanceable object to a string formatted as a point.
     * 
     * @param point To convert to string
     * @param unit The unit of the point
     * @return The string representation
     */
    public static String buildDistanceableString(IDistanceable point, String unit) {
        return String.format("(%1$.2f %3$s, %2$.2f %3$s)", point.getX(), point.getY(), unit);
    }
}
