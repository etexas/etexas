package com.harmonia.qa.ETEXASWebQATests.utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;

import com.cedarsoftware.util.io.JsonReader;
import com.cedarsoftware.util.io.JsonWriter;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasBaseEntity;

/**
 * Utility class to assist with IO files
 *
 * @author llaroussini
 */
public class ETexasFileUtils {

    /**
     * Accepts a list of entities and returns a JSON string representing those
     * objects
     *
     * @param list List of objects to serialize
     * @param <T> Type of objects contained in the list
     * @return a JSON string consisting of the fully serialized objects
     * @throws IOException if errors are encountered writing to JSON
     */
    public static <T> String writeToJSON(List<T> list) throws IOException {
        String jsonString = "";
        for (T entity : list) {
            jsonString = jsonString.concat(((ETexasBaseEntity)entity).getAsJSON());
        }
        return jsonString;
    }

    /**
     * Accepts a generic object, serializes it to JSON text which is then
     * written to the specified file.
     *
     * @param <T> The type of the object being serialized
     * @param t the object being serialized
     * @param file the file to which serialized data will be written
     * @throws IOException If errors are encountered in writing to the file
     */
    public static <T> void writeToFile(T t, File file) throws IOException {
        OutputStream output = new FileOutputStream(file);
        JsonWriter writer = new JsonWriter(output);
        writer.write(t);
        writer.close();
    }

    /**
     * Clears contents of a file without deleting the file itself.
     *
     * @param file the file of which the contents will be cleared
     * @throws FileNotFoundException If the given file object does not denote an
     *         existing, writable regular file and a new regular file of that
     *         name cannot be created, or if some other error occurs while
     *         opening or creating the file
     */
    public static void clearFileContents(File file) throws FileNotFoundException {
        PrintWriter writer = new PrintWriter(file);
        writer.write("null");
        writer.close();
    }

    /**
     * Reads JSON data from the passed file and returns an ArrayList of the
     * object contained in the file.
     *
     * @param file file object to be read containing serialized BaseEntity
     *        object data
     * @return A BaseEntity typed list of the objects contained in the JSON file
     * @throws IOException if errors are encountered opening the file
     */
    @SuppressWarnings("unchecked")
    public static List<ETexasBaseEntity> readFromJsonFile(File file) throws IOException {
        InputStream stream = FileUtils.openInputStream(file);
        JsonReader reader = new JsonReader(stream);
        List<ETexasBaseEntity> list = new ArrayList<ETexasBaseEntity>();
        list = (List<ETexasBaseEntity>)reader.readObject();
        reader.close();
        return list;
    }

    /**
     * Deserializes a list of Base Entities from a JSON file. If the JSON file
     * does not exist, it will be created initially as null.
     *
     * @param fileName path and name of a JSON file to be read containing
     *        serialized BaseEntity object data
     * @return A BaseEntity typed list of the objects contained in the JSON file
     * @throws IOException if errors are encountered opening the file
     */
    public static List<ETexasBaseEntity> readFromJsonFile(String fileName) throws IOException {
        File file = new File(fileName);
        if (!file.exists()) {
            OutputStream output = new FileOutputStream(file);
            JsonWriter writer = new JsonWriter(output);
            writer.write(null);
            writer.close();
        }
        return readFromJsonFile(file);
    }

}
