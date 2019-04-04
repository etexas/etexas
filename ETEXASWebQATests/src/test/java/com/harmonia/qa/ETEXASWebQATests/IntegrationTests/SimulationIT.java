package com.harmonia.qa.ETEXASWebQATests.IntegrationTests;

import static io.restassured.RestAssured.given;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import io.restassured.http.ContentType;
import io.restassured.response.Response;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.IntegrationTests.APIObjects.SimulationAPI;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Contains all integration testing for REST API simulation calls: WS-TC001
 *
 * @author abishop
 * @author llaroussini
 */
public class SimulationIT extends ETexasAfterTestResetTestBase {

    /**
     * The user needed for creating URLs and processing certain API calls within
     * the Simulation scope
     */
    private ETexasUser user;

    /**
     * A logged in user is required to create the proper URL for the simulation
     * portion of the REST API
     */
    @Before
    public void simulationWarmUp() {
        user = ETexasUserUtils.getLoggedInUser();
    }

    /**
     * 4.2.1. Create a New Template Simulation in a Composite (WS-TC001)
     */
    @Test
    public void createSimulationsFromTemplates() {
        Response response;

        //Test all known template examples with valid user names
        List<HashMap<String, String>> simulationMapList = SimulationAPI.getSimulationMapList();
        for (HashMap<String, String> simulationMap : simulationMapList) {
            response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
            assertEquals("Unexpected status code when valid simulation values from " + simulationMap.get(SimulationAPI.TEMPLATE) + " used.", 200, response.getStatusCode());
            assertNotNull("Unexpected response string when valid simulation values from " + simulationMap.get(SimulationAPI.TEMPLATE) + " used.", response.asString());

            //Test simulation with duplicate simulation name
            response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
            assertEquals("Unexpected status code when duplicate simulation name used with " + simulationMap.get(SimulationAPI.TEMPLATE), 409, response.getStatusCode());
            assertEquals("Unexpected error message when duplicate simulation name used with " + simulationMap.get(SimulationAPI.TEMPLATE),
                    SimulationAPI.generateDuplicateSimulationNameError(simulationMap.get(SimulationAPI.SIM_NAME)), response.asString());
        }

        //Test simulation creation with a composite name that contains invalid chars
        Map<String, String> simulationMap = SimulationAPI.getInvalidCharCompositeNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a composite name containing invalid chars and default template values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a composite name containing invalid chars and default template values used.", SimulationAPI.INVALID_COMPOSITE_NAME_ERROR, response.asString());

        //Test simulation creation with a composite name that contains inner consecutive whitespace
        simulationMap = SimulationAPI.getInnerWhitespaceCompositeNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a composite name with consecutive inner whitespace and default template values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a composite name with consecutive inner whitespace and default template values used.", SimulationAPI.INVALID_COMPOSITE_NAME_ERROR,
                response.asString());

        //Test simulation creation with a composite name that contains leading whitespace
        simulationMap = SimulationAPI.getLeadingWhitespaceCompositeNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a composite name with leading whitespace and default simulation values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a composite name with leading whitespace and default simulation values used.", SimulationAPI.WHITESPACE_COMPOSITE_NAME_ERROR, response.asString());

        //Test simulation creation with a composite name that contains trailing whitespace
        simulationMap = SimulationAPI.getTrailingWhitespaceCompositeNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a composite name with trailing whitespace and default simulation values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a composite name with trailing whitespace and default simulation values used.", SimulationAPI.WHITESPACE_COMPOSITE_NAME_ERROR, response.asString());

        //Test simulation creation with a composite name that contains only whitespace
        simulationMap = SimulationAPI.getWhitespaceCompositeNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a composite name with whitespace only and a valid template name used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a composite name with whitespace only and a valid template name used.",
                SimulationAPI.WHITESPACE_COMPOSITE_NAME_ERROR, response.asString());

        //Test simulation creation with a composite name that is an empty String
        simulationMap = SimulationAPI.getEmptyCompositeNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when empty String composite name and default template values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when empty String composite name and default template values used.", SimulationAPI.EMPTY_COMPOSITE_NAME_ERROR, response.asString());

        //Test simulation creation with a simulation name that contains invalid chars
        simulationMap = SimulationAPI.getInvalidCharSimNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a simulation name containing invalid chars and default template values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a simulation name containing invalid chars and default template values used.", SimulationAPI.INVALID_SIMULATION_NAME_ERROR, response.asString());

        //Test simulation creation with a simulation name that contains inner consecutive whitespace
        simulationMap = SimulationAPI.getInnerWhitespaceSimNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a simulation name with consecutive inner whitespace and default template values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a simulation name with consecutive inner whitespace and default template values used.", SimulationAPI.INVALID_SIMULATION_NAME_ERROR,
                response.asString());

        //Test simulation creation with a simulation name that contains leading whitespace
        simulationMap = SimulationAPI.getLeadingWhitespaceSimNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a simulation name with leading whitespace and default simulation values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a simulation name with leading whitespace and default simulation values used.", SimulationAPI.WHITESPACE_SIMULATION_NAME_ERROR, response.asString());

        //Test simulation creation with a simulation name that contains trailing whitespace
        simulationMap = SimulationAPI.getTrailingWhitespaceSimNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a simulaiton name with trailing whitespace and default simulation values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a simulation name with trailing whitespace and default simulation values used.", SimulationAPI.WHITESPACE_SIMULATION_NAME_ERROR,
                response.asString());

        //Test simulation creation with a name that contains only whitespace
        simulationMap = SimulationAPI.getWhitespaceSimNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a simulation name with whitespace only and default simulation values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a simulation name with whitespace only and default simulation values used.",
                SimulationAPI.WHITESPACE_SIMULATION_NAME_ERROR, response.asString());

        //Test simulation creation with a simulation name that is an empty String
        simulationMap = SimulationAPI.getEmptySimNameSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when empty String simulation name and default template values used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when empty String simulation name and default template values used.", SimulationAPI.EMPTY_SIMULATION_NAME_ERROR, response.asString());

        //Test simulation creation with an invalid template
        simulationMap = SimulationAPI.getInvalidTemplateSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a valid composite name, valid simulation name, andinvalid template value used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a valid composite name, valid simulation name, and invalid template value used.",
                SimulationAPI.generateInvalidTemplateNameError(simulationMap.get(SimulationAPI.TEMPLATE)), response.asString());

        //Test simulation creation with a template that contains only whitespace
        simulationMap = SimulationAPI.getWhitespaceTemplateSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a valid composite name, valid simulation name, and a template name containing only whitespace used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a valid composite name, valid simulation name, and a template name containing only whitespace used.",
                SimulationAPI.generateInvalidTemplateNameError(simulationMap.get(SimulationAPI.TEMPLATE)), response.asString());

        //Test simulation creation with a template that is an empty String
        simulationMap = SimulationAPI.getEmptyTemplateSimulationMap();
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when a valid composite name, valid simulation name, and an empty String template used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when a valid composite name, valid simulation name, and an empty String template used.", SimulationAPI.EMPTY_TEMPLATE_NAME_ERROR, response.asString());

    }

    /**
     * 4.2.4 Create Simulation from Template (WS-TC044)
     */
    //TODO Re-implement when updated for newly formatted REST calls
    //@Test
    public void getTemplateSimulations() {
        Response response;
        Map<String, String> simulationMap = SimulationAPI.getHappySimulationMap(user.getUsername());
        response = given().contentType(ContentType.URLENC).formParams(simulationMap).when().post(SimulationAPI.generateSimulationURL(user));
        assertEquals("Unexpected status code when valid simulation values from " + simulationMap.get(SimulationAPI.TEMPLATE) + " used.", 200, response.getStatusCode());
        assertNotNull("Unexpected response string when valid simulation values from " + simulationMap.get(SimulationAPI.TEMPLATE) + " used.", response.body().jsonPath().get(SimulationAPI.SIMID));
    }
}
