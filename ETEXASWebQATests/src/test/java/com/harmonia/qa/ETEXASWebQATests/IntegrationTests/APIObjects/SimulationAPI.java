package com.harmonia.qa.ETEXASWebQATests.IntegrationTests.APIObjects;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasProperties;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Contains logic specific to Simulation REST API integration tests
 *
 * @author abishop
 */
public class SimulationAPI {

    /**
     * simId parameter
     */
    public static final String SIMID = "simId";

    /**
     * Template parameter
     */
    public static final String TEMPLATE = "templateName";

    /**
     * Composite Name login parameter
     */
    public static final String COMPOSITE_NAME = "compositeName";

    /**
     * SimulationName login parameter
     */
    public static final String SIM_NAME = "simulationName";

    /**
     * Default template used for various test cases
     */
    public static final String DEFAULT_TEMPLATE = "exam_01";

    /**
     * Error message for an empty template name
     */
    public static final String EMPTY_TEMPLATE_NAME_ERROR = "Invalid Simulation Template;A valid template name is required.";

    /**
     * Error message for an empty composite name
     */
    public static final String EMPTY_COMPOSITE_NAME_ERROR = "Invalid Composite Name;A valid composite name is required.";

    /**
     * Error message for an empty simulation name
     */
    public static final String EMPTY_SIMULATION_NAME_ERROR = "Invalid Simulation Name;A valid simulation name is required.";

    /**
     * Error message for an invalid composite name
     */
    public static final String INVALID_COMPOSITE_NAME_ERROR = "Invalid Composite Name;Composite names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Error message for an invalid simulation name
     */
    public static final String INVALID_SIMULATION_NAME_ERROR = "Invalid Simulation Name;Simulation names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.";

    /**
     * Error message for an whitespace composite name
     */
    public static final String WHITESPACE_COMPOSITE_NAME_ERROR = "Invalid Composite Name;Composite names may not contain leading or trailing spaces.";

    /**
     * Error message for an whitespace simulation name
     */
    public static final String WHITESPACE_SIMULATION_NAME_ERROR = "Invalid Simulation Name;Simulation names may not contain leading or trailing spaces.";

    /**
     * Creates a list of all valid template names associated with eTEXAS
     *
     * @return list containing template names
     */
    public static List<String> getListOfTemplates() {
        List<String> templates = new LinkedList<String>();
        templates.add("exam_01");
        templates.add("exam_02");
        templates.add("exam_03");
        templates.add("exam_04");
        templates.add("exam_05");
        templates.add("exam_06");
        templates.add("exam_07");
        templates.add("exam_08");
        templates.add("exam_09");
        templates.add("exam_10");
        templates.add("exam_11");
        templates.add("exam_12");
        templates.add("exam_13");
        templates.add("exam_14");
        templates.add("exam_15");
        templates.add("exam_16");
        templates.add("exam_17");
        //templates.add("exam_18"); BUG 8992
        templates.add("exam_19");
        templates.add("exam_20");
        return templates;
    }

    /**
     * Generates a fully qualified simulation REST API URL containing a logged
     * in username and their respective session token
     *
     * @param user Logged in user
     * @return URL for simulation API calls for the given user
     */
    public static String generateSimulationURL(ETexasUser user) {
        String simulationEndpoint = "http://" + user.getUsername() + ":" + user.getToken() + "@" + ETexasProperties.hostName + "/rest/api/simulations";
        return simulationEndpoint;
    }

    /**
     * Generates a invalid simulation name error message
     *
     * @param simulationName used in the POST call
     * @return error message
     */
    public static String generateDuplicateSimulationNameError(String simulationName) {
        return "Add Simulation Failure;A simulation with the name \"" + simulationName + "\" already exists in the composite.";
    }

    /**
     * Generates a invalid template name error message
     *
     * @param templateName used in the POST call
     * @return error message
     */
    public static String generateInvalidTemplateNameError(String templateName) {
        return "Invalid Simulation Template;The value \"" + templateName + "\" is not a recognized template name.";
    }

    /**
     * Generates a List<Map<String, String> that includes all known examples for
     * simulation API calls
     *
     * @return List<HashMap<String, String, String>
     */
    public static List<HashMap<String, String>> getSimulationMapList() {
        List<HashMap<String, String>> simulationMapList = new LinkedList<HashMap<String, String>>();
        List<String> templates = getListOfTemplates();
        for (String template : templates) {
            HashMap<String, String> simulationMap = new HashMap<String, String>();
            simulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(10, false));
            simulationMap.put(SIM_NAME, RandomStringGenerator.nextString(10, false));
            simulationMap.put(TEMPLATE, template);
            simulationMapList.add(simulationMap);
        }
        return simulationMapList;
    }

    /**
     * Generates a HashMap containing a empty String composite name and the
     * default template
     *
     * @return blankUserSimulationMap
     */
    public static Map<String, String> getEmptyCompositeNameSimulationMap() {
        Map<String, String> blankUserSimulationMap = new HashMap<>();
        blankUserSimulationMap.put(COMPOSITE_NAME, "");
        blankUserSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(10, false));
        blankUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return blankUserSimulationMap;
    }

    /**
     * Generates a HashMap containing a empty String simulation name and the
     * default template
     *
     * @return blankUserSimulationMap
     */
    public static Map<String, String> getEmptySimNameSimulationMap() {
        Map<String, String> blankUserSimulationMap = new HashMap<>();
        blankUserSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(10, false));
        blankUserSimulationMap.put(SIM_NAME, "");
        blankUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return blankUserSimulationMap;
    }

    /**
     * Generates a HashMap containing a empty String template and the default
     * template
     *
     * @return blankUserSimulationMap
     */
    public static Map<String, String> getEmptyTemplateSimulationMap() {
        Map<String, String> blankUserSimulationMap = new HashMap<>();
        blankUserSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(10, false));
        blankUserSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(10, false));
        blankUserSimulationMap.put(TEMPLATE, "");
        return blankUserSimulationMap;
    }

    /**
     * Generates a HashMap containing a composite name with invalid characters
     * and the default template
     *
     * @return invalidUserSimulationMap
     */
    public static Map<String, String> getInvalidCharCompositeNameSimulationMap() {
        Map<String, String> invalidUserSimulationMap = new HashMap<>();
        invalidUserSimulationMap.put(COMPOSITE_NAME, "&" + RandomStringGenerator.nextString(10, false));
        invalidUserSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(10, false));
        invalidUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return invalidUserSimulationMap;
    }

    /**
     * Generates a HashMap containing a simulation name with invalid characters
     * and the default template
     *
     * @return invalidUserSimulationMap
     */
    public static Map<String, String> getInvalidCharSimNameSimulationMap() {
        Map<String, String> invalidUserSimulationMap = new HashMap<>();
        invalidUserSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(10, false));
        invalidUserSimulationMap.put(SIM_NAME, "&" + RandomStringGenerator.nextString(10, false));
        invalidUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return invalidUserSimulationMap;
    }

    /**
     * Generates a HashMap containing an invalid template
     *
     * @return invalidUserSimulationMap
     */
    public static Map<String, String> getInvalidTemplateSimulationMap() {
        Map<String, String> invalidUserSimulationMap = new HashMap<>();
        invalidUserSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(10, false));
        invalidUserSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(10, false));
        invalidUserSimulationMap.put(TEMPLATE, RandomStringGenerator.nextString(10, false));
        return invalidUserSimulationMap;
    }

    /**
     * Generates a HashMap containing a valid name and a template name
     * containing only whitespace
     *
     * @return blankTemplateSimulationMap
     */
    public static Map<String, String> getWhitespaceCompositeNameSimulationMap() {
        Map<String, String> blankTemplateSimulationMap = new HashMap<>();
        blankTemplateSimulationMap.put(COMPOSITE_NAME, "   ");
        blankTemplateSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(10, false));
        blankTemplateSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return blankTemplateSimulationMap;
    }

    /**
     * Generates a HashMap containing a valid name and a template name
     * containing only whitespace
     *
     * @return blankTemplateSimulationMap
     */
    public static Map<String, String> getWhitespaceSimNameSimulationMap() {
        Map<String, String> blankTemplateSimulationMap = new HashMap<>();
        blankTemplateSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(10, false));
        blankTemplateSimulationMap.put(SIM_NAME, "   ");
        blankTemplateSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return blankTemplateSimulationMap;
    }

    /**
     * Generates a HashMap containing a valid name and a template name
     * containing only whitespace
     *
     * @return blankTemplateSimulationMap
     */
    public static Map<String, String> getWhitespaceTemplateSimulationMap() {
        Map<String, String> blankTemplateSimulationMap = new HashMap<>();
        blankTemplateSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(10, false));
        blankTemplateSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(10, false));
        blankTemplateSimulationMap.put(TEMPLATE, "   ");
        return blankTemplateSimulationMap;
    }

    /**
     * Generates a HashMap containing a composite name with consecutive
     * whitespace surrounded by valid chars and the default template
     *
     * @return blankTemplateSimulationMap
     */
    public static Map<String, String> getInnerWhitespaceCompositeNameSimulationMap() {
        Map<String, String> spacesUserSimulationMap = new HashMap<>();
        spacesUserSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(4, false) + "  " + RandomStringGenerator.nextString(4, false));
        spacesUserSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(10, false));
        spacesUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return spacesUserSimulationMap;
    }

    /**
     * Generates a HashMap containing a name with consecutive whitespace
     * surrounded by valid chars and the default template
     *
     * @return blankTemplateSimulationMap
     */
    public static Map<String, String> getInnerWhitespaceSimNameSimulationMap() {
        Map<String, String> spacesUserSimulationMap = new HashMap<>();
        spacesUserSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(10, false));
        spacesUserSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(4, false) + "  " + RandomStringGenerator.nextString(4, false));
        spacesUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return spacesUserSimulationMap;
    }

    /**
     * Generates a HashMap containing a composite name with leading whitespace
     * and the default template
     *
     * @return blankTemplateSimulationMap
     */
    public static Map<String, String> getLeadingWhitespaceCompositeNameSimulationMap() {
        Map<String, String> leadingSpacesUserSimulationMap = new HashMap<>();
        leadingSpacesUserSimulationMap.put(COMPOSITE_NAME, "  " + RandomStringGenerator.nextString(9, false));
        leadingSpacesUserSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(9, false));
        leadingSpacesUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return leadingSpacesUserSimulationMap;
    }

    /**
     * Generates a HashMap containing a simulation name with leading whitespace
     * and the default template
     *
     * @return blankTemplateSimulationMap
     */
    public static Map<String, String> getLeadingWhitespaceSimNameSimulationMap() {
        Map<String, String> leadingSpacesUserSimulationMap = new HashMap<>();
        leadingSpacesUserSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(9, false));
        leadingSpacesUserSimulationMap.put(SIM_NAME, "  " + RandomStringGenerator.nextString(9, false));
        leadingSpacesUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return leadingSpacesUserSimulationMap;
    }

    /**
     * Generates a HashMap containing a composite name with trailing whitespace
     * and the default template
     *
     * @return blankTemplateSimulationMap
     */
    public static Map<String, String> getTrailingWhitespaceCompositeNameSimulationMap() {
        Map<String, String> leadingSpacesUserSimulationMap = new HashMap<>();
        leadingSpacesUserSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(9, false) + "  ");
        leadingSpacesUserSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(10, false));
        leadingSpacesUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return leadingSpacesUserSimulationMap;
    }

    /**
     * Generates a HashMap containing a simulation name with trailing whitespace
     * and the default template
     *
     * @return blankTemplateSimulationMap
     */
    public static Map<String, String> getTrailingWhitespaceSimNameSimulationMap() {
        Map<String, String> leadingSpacesUserSimulationMap = new HashMap<>();
        leadingSpacesUserSimulationMap.put(COMPOSITE_NAME, RandomStringGenerator.nextString(10, false));
        leadingSpacesUserSimulationMap.put(SIM_NAME, RandomStringGenerator.nextString(9, false) + "  ");
        leadingSpacesUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return leadingSpacesUserSimulationMap;
    }

    /**
     * Creates a happy path simulation map containing the user passed in and the
     * default template
     *
     * @param username whos account the template will be attached to
     * @return Parameter Map for accessing the Simulation URL
     */
    public static Map<String, String> getHappySimulationMap(String username) {
        Map<String, String> leadingSpacesUserSimulationMap = new HashMap<>();
        leadingSpacesUserSimulationMap.put(SIM_NAME, username);
        leadingSpacesUserSimulationMap.put(TEMPLATE, DEFAULT_TEMPLATE);
        return leadingSpacesUserSimulationMap;
    }
}
