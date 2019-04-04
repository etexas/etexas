package com.harmonia.qa.ETEXASWebQATests.IntegrationTests.APIObjects;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.harmonia.qa.ETEXASWebQATests.entities.UserNativeApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserRemoteApp;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.AppFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasProperties;

/**
 * Contains logic specific to Simulation REST API integration tests
 *
 * @author abishop
 */
public class AppManagementAPI {

    /**
     * Enum containing all the REST API parameters names, and the names
     * contained in server responses when querying applications for a user for
     * application management
     *
     * @author abishop
     */
    public enum AppParameters {
        /**
         * Application name parameter/server response name
         */
        APPLICATION_NAME("applicationName", "name"),

        /**
         * Device type parameter/server response name
         */
        DEVICE_TYPE("deviceType", "deviceType"),

        /**
         * Command line parameter/server response name
         */
        COMMAND_LINE("commandLine", "commandLine"),

        /**
         * Host address parameter/server response name
         */
        HOST_ADDRESS("hostAddress", "hostAddress"),

        /**
         * Port number parameter/server response name
         */
        PORT_NUMBER("portNumber", "portNumber"),

        /**
         * ID this is not a parameter, but a returned value from the server that
         * must be stored for URL creation and server response validation/server
         * response name
         */
        ID("applicationId", "id"),

        /**
         * Application type, this is not a parameter, but a returned value from
         * the server that must be stored for server response validation/server
         * response name
         */
        APPLICATION_TYPE("type", "type");

        /**
         * The name of the parameter used for application management REST calls
         */
        private String parameter;

        /**
         * The key for the string value contained in the JSON response from the
         * eTEXAS server
         */
        private String jsonName;

        /**
         * Default constructor; sets the parameter and jsonName values
         *
         * @param parameter the string value of the parameter
         * @param jsonName the string value of the JSON key returned by the
         *        eTEXAS server
         */
        AppParameters(String parameter, String jsonName) {
            this.parameter = parameter;
            this.jsonName = jsonName;
        }

        /**
         * Gets the string value expected by the server when making REST calls
         *
         * @return The label of the application type
         */
        public String getParameter() {
            return this.parameter;
        }

        /**
         * Gets the string value returned by the server when making GET calls
         *
         * @return The label of the application type
         */
        public String getJsonName() {
            return this.jsonName;
        }
    }

    /**
     * Applications URL
     */
    public static final String APPLICATIONS_ENDPOINT = ETexasProperties.hostName + "/rest/api/applications";

    /**
     * Application parameters URL
     */
    public static final String APPLICATIONS_PARAMETERS_ENDPOINT = APPLICATIONS_ENDPOINT + "/parameters/";

    /**
     * Jar applications URL
     */
    public static final String JAR_APPLICATIONS_ENDPOINT = APPLICATIONS_ENDPOINT + "/jar/";

    /**
     * Native application URL
     */
    public static final String NATIVE_APPLICATIONS_ENDPOINT = APPLICATIONS_ENDPOINT + "/native/";

    /**
     * Remote applications URL
     */
    public static final String REMOTE_APPLICATIONS_ENDPOINT = APPLICATIONS_ENDPOINT + "/remote/";

    /**
     * Delete application URL
     */
    public static final String DELETE_APPLICATIONS_ENDPOINT = APPLICATIONS_ENDPOINT + "/";

    /**
     * Invalid application name error
     */
    public static final String INVALID_APP_NAME_ERROR = "Invalid Application Name;Application names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Invalid command line error
     */
    public static final String INVALID_COMMAND_LINE_ERROR = "Invalid Command Line;Command lines may contain only letters, digits, hyphens, periods, underscores, and nonconsecutive spaces.";

    /**
     * Invalid port error
     */
    public static final String INVALID_PORT_ERROR = "Invalid Port Number;Port numbers must be in the range of 1 to 65535.";

    /**
     * Blank device type error
     */
    public static final String BLANK_DEVICE_TYPE_ERROR = "Invalid Device Type;A valid device type is required.";

    /**
     * Blank application name error
     */
    public static final String BLANK_APP_NAME_ERROR = "Invalid Application Name;A valid application name is required.";

    /**
     * Blank command line error
     */
    public static final String BLANK_COMMAND_LINE_ERROR = "Invalid Command Line;A valid command line is required.";

    /**
     * Blank host address error
     */
    public static final String BLANK_HOST_ERROR = "Invalid Host Address;A valid host address is required.";

    /**
     * Generate a parameter map containing valid values for a native application
     *
     * @return valid native application parameter map HashMap RestAssured
     *         digests Map one entry (<String, String>) at a time. The first
     *         string represents the parameter name, the second string
     *         represents the parameter value. It reads the entries
     *         procedurally, so it is important to maintain the order expected
     *         by the API being used
     */
    public static Map<String, String> getHappyNativeAppParameterMap() {

        Map<String, String> nativeAppParametersMap = new HashMap<>();

        UserNativeApp nativeApp = AppFactory.getUserNativeApp(true);

        nativeAppParametersMap.put(AppParameters.APPLICATION_NAME.parameter, nativeApp.getName());
        nativeAppParametersMap.put(AppParameters.DEVICE_TYPE.parameter, nativeApp.getDeviceType().getRESTLabel());
        nativeAppParametersMap.put(AppParameters.COMMAND_LINE.parameter, nativeApp.getCommandLine());
        nativeAppParametersMap.put(AppParameters.HOST_ADDRESS.parameter, nativeApp.getHost());
        nativeAppParametersMap.put(AppParameters.PORT_NUMBER.parameter, nativeApp.getPort());

        return nativeAppParametersMap;
    }

    /**
     * Generate a parameter map containing empty string values for a native
     * application
     *
     * @return empty string native application parameter map
     */
    public static Map<String, String> getEmptyStringNativeParameterMap() {

        Map<String, String> emptyStringNativeAppParametersMap = new HashMap<>();

        emptyStringNativeAppParametersMap.put(AppParameters.APPLICATION_NAME.parameter, "");
        emptyStringNativeAppParametersMap.put(AppParameters.DEVICE_TYPE.parameter, "");
        emptyStringNativeAppParametersMap.put(AppParameters.COMMAND_LINE.parameter, "");
        emptyStringNativeAppParametersMap.put(AppParameters.HOST_ADDRESS.parameter, "");
        emptyStringNativeAppParametersMap.put(AppParameters.PORT_NUMBER.parameter, "");

        return emptyStringNativeAppParametersMap;
    }

    /**
     * Generate a valid parameter map for updating a native application
     *
     * @return valid parameter map for updating a native application
     */
    public static Map<String, String> getHappyUpdatedNativeAppParameterMap() {

        Map<String, String> updatedNativeAppParametersMap = getHappyNativeAppParameterMap();
        updatedNativeAppParametersMap.remove(AppParameters.DEVICE_TYPE.parameter);

        return updatedNativeAppParametersMap;
    }

    /**
     * Generate a parameter map for a native application with an invalid device
     * type
     *
     * @param invalidValue the invalid value to be used for deviceType
     * @return parameter map for a native application with an invalid device
     *         type
     */
    public static Map<String, String> getInvalidDeviceNativeParameterMap(String invalidValue) {

        Map<String, String> invalidDeviceNativeAppParametersMap = getHappyNativeAppParameterMap();
        invalidDeviceNativeAppParametersMap.put(AppParameters.DEVICE_TYPE.parameter, invalidValue);

        return invalidDeviceNativeAppParametersMap;
    }

    /**
     * Generate a parameter map for a native application with an invalid
     * application name
     *
     * @param invalidValue the invalid value to be used for aplicationName
     * @return parameter map for a native application with an invalid
     *         application name
     */
    public static Map<String, String> getInvalidAppNameNativeParameterMap(String invalidValue) {

        Map<String, String> invalidAppNameNativeAppParametersMap = getHappyNativeAppParameterMap();
        invalidAppNameNativeAppParametersMap.put(AppParameters.APPLICATION_NAME.parameter, invalidValue);

        return invalidAppNameNativeAppParametersMap;
    }

    /**
     * Generate a parameter map for a native application with an invalid command
     * line
     *
     * @param invalidValue the invalid value to be used for commandLine
     * @return parameter map for a native application with an invalid command
     *         line
     */
    public static Map<String, String> getInvalidCommandLineNativeParameterMap(String invalidValue) {

        Map<String, String> invalidAppNameNativeAppParametersMap = getHappyNativeAppParameterMap();
        invalidAppNameNativeAppParametersMap.put(AppParameters.COMMAND_LINE.parameter, invalidValue);

        return invalidAppNameNativeAppParametersMap;
    }

    /**
     * Generate a parameter map for a native application with an invalid host
     * address
     *
     * @param invalidValue the invalid value to be used for host
     * @return parameter map for a native application with an invalid host
     *         address
     */
    public static Map<String, String> getInvalidHostNativeParameterMap(String invalidValue) {

        Map<String, String> invalidHostNativeAppParametersMap = getHappyNativeAppParameterMap();
        invalidHostNativeAppParametersMap.put(AppParameters.HOST_ADDRESS.parameter, invalidValue);

        return invalidHostNativeAppParametersMap;
    }

    /**
     * Generate a parameter map for a native application with an invalid port
     *
     * @param invalidValue the invalid value to be used for port
     * @return parameter map for a native application with an invalid port
     */
    public static Map<String, String> getInvalidPortNativeParameterMap(String invalidValue) {

        Map<String, String> invalidPortNativeAppParametersMap = getHappyNativeAppParameterMap();
        invalidPortNativeAppParametersMap.put(AppParameters.PORT_NUMBER.parameter, invalidValue);

        return invalidPortNativeAppParametersMap;
    }

    /**
     * Generate a parameter map containing values for a valid remote application
     *
     * @return parameter map containing values for a valid remote application
     *         HashMap RestAssured digests Map one entry (<String, String>) at a
     *         time. The first string represents the parameter name, the second
     *         string represents the parameter value. It reads the entries
     *         procedurally, so it is important to maintain the order expected
     *         by the API being used
     */
    public static Map<String, String> getHappyRemoteAppParameterMap() {

        Map<String, String> happyRemoteAppParametersMap = new HashMap<>();

        UserRemoteApp remoteApp = AppFactory.getUserRemoteApp(true);

        happyRemoteAppParametersMap.put(AppParameters.APPLICATION_NAME.parameter, remoteApp.getName());
        happyRemoteAppParametersMap.put(AppParameters.DEVICE_TYPE.parameter, remoteApp.getDeviceType().getRESTLabel());

        return happyRemoteAppParametersMap;
    }

    /**
     * Generate a parameter map for updating a remote application with valid
     * values
     *
     * @return parameter map for updating a remote application with valid values
     */
    public static Map<String, String> getHappyUpdatedRemoteAppParameterMap() {

        Map<String, String> updatedRemoteAppParametersMap = getHappyRemoteAppParameterMap();
        updatedRemoteAppParametersMap.remove(AppParameters.DEVICE_TYPE.parameter);

        return updatedRemoteAppParametersMap;
    }

    /**
     * Generate a parameter map containing values for a remote application with
     * an invalid device type
     *
     * @param invalidValue the invalid value to be used for deviceType
     * @return parameter map containing values for a remote application with an
     *         invalid device type
     */
    public static Map<String, String> getInvalidDeviceRemoteAppParameterMap(String invalidValue) {

        Map<String, String> invalidDeviceRemoteAppParametersMap = getHappyRemoteAppParameterMap();
        invalidDeviceRemoteAppParametersMap.put(AppParameters.DEVICE_TYPE.parameter, invalidValue);

        return invalidDeviceRemoteAppParametersMap;
    }

    /**
     * Generate a parameter map containing values for remote application with an
     * invalid application name
     *
     * @param invalidValue the invalid value to be used for aplicationName
     * @return parameter map containing values for remote application with an
     *         invalid application name
     */
    public static Map<String, String> getInvalidAppNameRemoteAppParameterMap(String invalidValue) {

        Map<String, String> invalidAppNameRemoteAppParametersMap = getHappyRemoteAppParameterMap();
        invalidAppNameRemoteAppParametersMap.put(AppParameters.APPLICATION_NAME.parameter, invalidValue);

        return invalidAppNameRemoteAppParametersMap;
    }

    /**
     * Generate a parameter map for a remote application containing empty
     * strings for parameter values
     *
     * @return parameter map for a remote application containing empty strings
     *         for parameter values
     */
    public static Map<String, String> getBlankRemoteAppParameterMap() {

        Map<String, String> blankRemoteAppParametersMap = new HashMap<>();

        blankRemoteAppParametersMap.put(AppParameters.APPLICATION_NAME.parameter, "");
        blankRemoteAppParametersMap.put(AppParameters.DEVICE_TYPE.parameter, "");

        return blankRemoteAppParametersMap;
    }

    /**
     * Generate a parameter map for updating a remote application containing an
     * empty string for the parameter value
     *
     * @return parameter map for updating a remote application containing an
     *         empty string for the parameter value
     */
    public static Map<String, String> getBlankUpdatedRemoteAppParameterMap() {

        Map<String, String> blankRemoteAppParametersMap = new HashMap<>();
        blankRemoteAppParametersMap.put(AppParameters.APPLICATION_NAME.parameter, "");

        return blankRemoteAppParametersMap;
    }

    /**
     * Generates a URL for the given user's logingParametersMaps and the given
     * URL suffix
     *
     * @param loginParametersMap the user associated with the desired REST call
     * @param applicationsEndpoint the suffix associated with the desired REST
     *        call
     * @return String representation of the generated URL
     */
    public static String generateApplicationsEndpoint(Map<String, String> loginParametersMap, String applicationsEndpoint) {
        String userName = loginParametersMap.get(UserAPI.USERNAME);
        String token = loginParametersMap.get(UserAPI.TOKEN);
        String applicationsURI = "http://" + userName + ":" + token + "@" + applicationsEndpoint;
        return applicationsURI;
    }

    /**
     * Generates a URL for the given user's logingParametersMaps, the given URL
     * suffix, and the given application ID
     *
     * @param loginParametersMap the user associated with the desired REST call
     * @param applicationsEndpoint the suffix associated with the desired REST
     *        call
     * @param appId the ID of the application being modified
     * @return String representation of the generated URL
     */
    public static String generateApplicationsEndpoint(Map<String, String> loginParametersMap, String applicationsEndpoint, String appId) {
        String userName = loginParametersMap.get(UserAPI.USERNAME);
        String token = loginParametersMap.get(UserAPI.TOKEN);
        String applicationsURI = "http://" + userName + ":" + token + "@" + applicationsEndpoint + appId;
        return applicationsURI;
    }

    /**
     * Captures an application ID after it has been successfully uploaded to the
     * eTEXAS server
     *
     * @param appParameterMap parameter map for the application that was created
     * @param appId the ID assigned to the application by the eTEXAS server
     * @return the updated appParameterMap
     */
    public static Map<String, String> captureAppId(Map<String, String> appParameterMap, String appId) {
        appParameterMap.put(AppParameters.ID.parameter, appId);
        return appParameterMap;
    }

    /**
     * Parses the raw JSON response returned from GET calls to the Application
     * endpoint
     *
     * @param string the JSON string that was returned from the server
     * @return a JsonNode that contains only the values that need to be verified
     */
    public static JsonNode parseGetApplicationResponse(String string) {
        ObjectMapper objectMapper = new ObjectMapper();
        JsonNode test = null;
        try {
            test = objectMapper.readTree(string);
            test = test.get("list");
            Iterator<JsonNode> iterator = test.iterator();
            test = iterator.next();
        }
        catch (JsonProcessingException e) {
            e.printStackTrace();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        return test;
    }

    /**
     * Error message returned by the eTEXAS server when attempting to create a
     * native application with a duplicate application name
     *
     * @param negativeNativeAppParameterMap used to produce the error message
     * @return the error message
     */
    public static String generateDuplicateNativeAppNameError(Map<String, String> negativeNativeAppParameterMap) {
        return "Add Native Application Failure;An application with the name \"" + negativeNativeAppParameterMap.get(AppParameters.APPLICATION_NAME.parameter) + "\" already exists.";
    }

    /**
     * Error message returned by the eTEXAS server when attempting to create a
     * remote application with a duplicate application name
     *
     * @param negativeNativeAppParameterMap used to produce the error message
     * @return the error message
     */
    public static String generateDuplicateRemoteAppNameError(Map<String, String> negativeNativeAppParameterMap) {
        return "Add Remote Application Failure;An application with the name \"" + negativeNativeAppParameterMap.get(AppParameters.APPLICATION_NAME.parameter) + "\" already exists.";
    }

    /**
     * Error message returned by the eTEXAS server when attempting to create an
     * application with an invalid device type
     *
     * @param negativeNativeAppParameterMap used to produce the error message
     * @return the error message
     */
    public static String generateInvalidDeviceError(Map<String, String> negativeNativeAppParameterMap) {
        return "Invalid Device Type;The value \"" + negativeNativeAppParameterMap.get(AppParameters.DEVICE_TYPE.parameter) + "\" is not a recognized device type.";
    }

    /**
     * Error message returned by the eTEXAS server when attempting to create an
     * application with an invalid host address
     *
     * @param negativeNativeAppParameterMap used to produce the error message
     * @return the error message
     */
    public static String generateInvalidHostError(Map<String, String> negativeNativeAppParameterMap) {
        return "Invalid Host Address;The value \"" + negativeNativeAppParameterMap.get(AppParameters.HOST_ADDRESS.parameter) + "\" is not a recognized IP address.";
    }

    /**
     * Error message returned by the eTEXAS server when attempting to modify an
     * application with an invalid application ID
     *
     * @param invalidId used to produce the error message
     * @return the error message
     */
    public static String generateInvalidAppIdError(String invalidId) {
        return "Remove Application Failure;No application with ID \"" + invalidId + "\" could be found.";
    }
}
