package com.harmonia.qa.ETEXASWebQATests.IntegrationTests;

import static io.restassured.RestAssured.given;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import io.restassured.http.ContentType;
import io.restassured.response.Response;

import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.harmonia.qa.ETEXASWebQATests.IntegrationTests.APIObjects.AppManagementAPI;
import com.harmonia.qa.ETEXASWebQATests.IntegrationTests.APIObjects.UserAPI;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;

/**
 * Tests all paths associated with application management through the REST API
 * (WS-TC050 WS-TC044 WS-TC015 WS-TC017 WS-TC011 WS-TC072) TODO jar applications
 * can only be updated through the API, therefore we can not easily put in
 * testing for that particular REST call. If adding jar applications is
 * implemented again in the future, we will need to test the update
 * functionality
 *
 * @author abishop
 */
public class AppManagementIT extends ETexasAfterTestResetTestBase {

    /**
     * Container for server responses
     */
    private Response response;

    /**
     * Container for a valid parameter map required to login an eTEXAS user
     */
    private Map<String, String> userParametersMap;

    /**
     * Container for a invalid parameter map required to login an eTEXAS user
     */
    private Map<String, String> invalidUserParametersMap;

    /**
     * An invalid application ID for negative testing
     */
    private static final String invalidId = "12345";

    /**
     * Tests all aspects of the native application and remote application
     * portions of the API
     */
    @Before
    public void warmUp() {

        //Get a logged in user and build the user parameter map
        ETexasUser user = ETexasUserUtils.getLoggedInUser();
        userParametersMap = UserAPI.getUserParameterMap(user);
        userParametersMap = UserAPI.captureUserToken(userParametersMap, user.getToken());

        //Generate an invalid login parameter map for negative testing against the REST API authentication process
        invalidUserParametersMap = UserAPI.getHappyUserParameterMap();
        invalidUserParametersMap = UserAPI.captureUserToken(invalidUserParametersMap, "invalidToken");
    }

    /**
     * Tests all aspects of the native application portion of the REST API
     * WS-TC050 WS-TC044 WS-TC015 WS-TC017
     */
    @Test
    public void nativeApplicationsTest() {

        //Generate a native application parameter map and assign the application to the logged in user WS-TC050
        Map<String, String> nativeAppParameterMap = AppManagementAPI.getHappyNativeAppParameterMap();
        Map<String, String> negativeNativeAppParameterMap = nativeAppParameterMap;
        String nativeAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.NATIVE_APPLICATIONS_ENDPOINT);
        response = given().contentType(ContentType.URLENC).formParams(nativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a valid native application.", 200, response.getStatusCode());
        assertNotNull("Unexpected server reponse when adding a valid native application.", response.body());
        nativeAppParameterMap = AppManagementAPI.captureAppId(nativeAppParameterMap, response.body().asString());

        //Get a list of applications attached to this user which should be a list of one, the one we just created
        String getAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.APPLICATIONS_ENDPOINT);
        response = given().contentType(ContentType.URLENC).when().get(getAppEndpoint);
        assertEquals("Unexpected status code when getting a populated valid native application list.", 200, response.getStatusCode());
        assertEquals("Unexpected server response when getting a populated valid native application.", "application/json", response.contentType());

        //Verify server response matches the generated native application we added WS-TC015
        JsonNode serverReponse = AppManagementAPI.parseGetApplicationResponse(response.body().asString());
        for (AppManagementAPI.AppParameters appParameter : AppManagementAPI.AppParameters.values()) {
            if (nativeAppParameterMap.containsKey(appParameter.getParameter())) {
                String parameterMapValue = nativeAppParameterMap.get(appParameter.getParameter()).toLowerCase();
                String serverResponseValue = serverReponse.findValue(appParameter.getJsonName()).asText().toLowerCase();
                assertEquals("Genterated native application value " + appParameter.getParameter() + ", did not match the server reponse.", parameterMapValue, serverResponseValue);
            }
        }

        //Attempt to get a list of applications attached to this user with an invalid token in the URL WS-TC050
        getAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(invalidUserParametersMap, AppManagementAPI.APPLICATIONS_ENDPOINT);
        response = given().contentType(ContentType.URLENC).when().get(getAppEndpoint);
        assertEquals("Unexpected status code when getting a native application list with an invalid user token.", 401, response.getStatusCode());

        //Attempt to create a native application with the same name as the application that was just registered
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with a duplicate name.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with a duplicate name.", AppManagementAPI.generateDuplicateNativeAppNameError(negativeNativeAppParameterMap),
                response.asString());

        //Attempt to create a native application with an invalid device type
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidDeviceNativeParameterMap("invalid device type");
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an invalid device type.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an invalid device type.", AppManagementAPI.generateInvalidDeviceError(negativeNativeAppParameterMap),
                response.asString());

        //Attempt to create a native application with an invalid application name
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidAppNameNativeParameterMap("@ invalid application name #");
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an invalid application name.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an invalid application name.", AppManagementAPI.INVALID_APP_NAME_ERROR, response.asString());

        //Attempt to create a native application with an invalid command line
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidCommandLineNativeParameterMap("# invalid command line @");
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an invalid command line.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an invalid command line.", AppManagementAPI.INVALID_COMMAND_LINE_ERROR, response.asString());

        //Attempt to create a native application with an invalid host address
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidHostNativeParameterMap("invalid host address");
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an invalid host address.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an invalid host address.", AppManagementAPI.generateInvalidHostError(negativeNativeAppParameterMap),
                response.asString());

        //Attempt to create a native application with an invalid port
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidPortNativeParameterMap("0");
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an invalid port.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an invalid host address.", AppManagementAPI.INVALID_PORT_ERROR, response.asString());

        //Attempt to create a native application with empty strings for all parameter values
        negativeNativeAppParameterMap = AppManagementAPI.getEmptyStringNativeParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an invalid host name.", 400, response.getStatusCode());

        //Attempt to create a native application with an empty string device type
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidDeviceNativeParameterMap("");
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an empty string device type.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an an empty string device type.", AppManagementAPI.BLANK_DEVICE_TYPE_ERROR, response.asString());

        //Attempt to create a native application with an empty string application name
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidAppNameNativeParameterMap("");
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an empty string application name.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an an empty string application name.", AppManagementAPI.BLANK_APP_NAME_ERROR, response.asString());

        //Attempt to create a native application with an empty string command line
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidCommandLineNativeParameterMap("");
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an empty string command line.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an an empty string command line.", AppManagementAPI.BLANK_COMMAND_LINE_ERROR, response.asString());

        //Attempt to create a native application with an empty string host address
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidHostNativeParameterMap("");
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an empty string host address.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an an empty string host address.", AppManagementAPI.BLANK_HOST_ERROR, response.asString());

        //Attempt to create a native application with an empty string port
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidPortNativeParameterMap("");
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an empty string port.", 400, response.getStatusCode());

        //Attempt to create a native application with an empty string device type - Null checks are not possible in manual testing.
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidDeviceNativeParameterMap(null);
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an empty string device type.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an an empty string device type.", AppManagementAPI.BLANK_DEVICE_TYPE_ERROR, response.asString());

        //Attempt to create a native application with an empty string application name - Null checks are not possible in manual testing.
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidAppNameNativeParameterMap(null);
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an empty string application name.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a native application with an an empty string application name.", AppManagementAPI.BLANK_APP_NAME_ERROR, response.asString());

        //Attempt to create a native application with an empty string command line - Null checks are not possible in manual testing.
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidCommandLineNativeParameterMap(null);
        response = given().contentType(ContentType.URLENC).params(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an empty string command line.", 409, response.getStatusCode());
        //TODO Investigate why RestAssured produces different error messages than direct calls through REST Console
        //assertEquals("Unexpected error message when adding a native application with an an empty string command line.", AppManagementAPI.BLANK_COMMAND_LINE_ERROR, response.asString());

        //Attempt to create a native application with an empty string host address - Null checks are not possible in manual testing.
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidHostNativeParameterMap(null);
        response = given().contentType(ContentType.URLENC).params(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an empty string host address.", 409, response.getStatusCode());
        //TODO Investigate why RestAssured produces different error messages than direct calls through REST Console
        //assertEquals("Unexpected error message when adding a native application with an an empty string host address.", AppManagementAPI.BLANK_HOST_ERROR, response.asString()); BUG 13311

        //Attempt to create a native application with an empty string port - Null checks are not possible in manual testing.
        negativeNativeAppParameterMap = AppManagementAPI.getInvalidPortNativeParameterMap(null);
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().post(nativeAppEndpoint);
        assertEquals("Unexpected status code when adding a native application with an empty string port.", 409, response.getStatusCode());

        //Test the get parameters REST call
        String id = nativeAppParameterMap.get(AppManagementAPI.AppParameters.ID.getParameter());
        String appParamsEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.APPLICATIONS_PARAMETERS_ENDPOINT, id);
        response = given().contentType(ContentType.URLENC).when().get(appParamsEndpoint);
        assertEquals("Unexpected status code when getting valid native application params.", 200, response.getStatusCode());
        assertEquals("Unexpected response when getting valid native application params.", "application/json", response.contentType());

        //Update the native application and verify WS-TC044
        String updateNativeAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.NATIVE_APPLICATIONS_ENDPOINT, id);
        Map<String, String> updatedNativeAppParameterMap = AppManagementAPI.getHappyUpdatedNativeAppParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(updatedNativeAppParameterMap).when().put(updateNativeAppEndpoint);
        assertEquals("Unexpected status code when updating a valid native application.", 204, response.getStatusCode());

        //Attempt to update with an empty string parameter map
        negativeNativeAppParameterMap = AppManagementAPI.getEmptyStringNativeParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(negativeNativeAppParameterMap).when().put(updateNativeAppEndpoint);
        assertEquals("Unexpected status code when updating a native application with empty strings for parameter values", 400, response.getStatusCode());

        //Test application delete WS-TC017
        String deleteAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.DELETE_APPLICATIONS_ENDPOINT, id);
        response = given().contentType(ContentType.URLENC).when().delete(deleteAppEndpoint);
        assertEquals("Unexpected status code when deleting a valid native application.", 204, response.getStatusCode());

        //Attempt to delete an application with an invalid application ID
        deleteAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.DELETE_APPLICATIONS_ENDPOINT, invalidId);
        response = given().contentType(ContentType.URLENC).when().delete(deleteAppEndpoint);
        assertEquals("Unexpected status code when updating a native application with an an invalid ap id.", 404, response.getStatusCode());
        assertEquals("Unexpected error message when delting a native application with an an invalid ap id.", AppManagementAPI.generateInvalidAppIdError(invalidId), response.asString());
    }

    /**
     * Tests all aspects of the remote application portion of the REST API
     * WS-TC011 WS-TC072 WS-TC015 WS-TC017
     */
    @Test
    public void remoteApplicationsTest() {

        //Generate a remote application parameter map and assign the application to the logged in user WS-TC011
        Map<String, String> remoteAppParameterMap = AppManagementAPI.getHappyRemoteAppParameterMap();
        Map<String, String> negativeRemoteParamterMap = remoteAppParameterMap;
        String remoteAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.REMOTE_APPLICATIONS_ENDPOINT);
        response = given().contentType(ContentType.URLENC).formParams(remoteAppParameterMap).when().post(remoteAppEndpoint);
        assertEquals("Unexpected status code when adding a valid remote application.", 200, response.getStatusCode());
        assertNotNull("Unexpected server reponse when adding a valid remote application.", response.body());
        remoteAppParameterMap = AppManagementAPI.captureAppId(remoteAppParameterMap, response.body().asString());

        //Get a list of applications attached to this use which should be a list of one application, the one we just created
        String getAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.APPLICATIONS_ENDPOINT);
        response = given().contentType(ContentType.URLENC).when().get(getAppEndpoint);
        assertEquals("Unexpected status code when getting a valid remote application list.", 200, response.getStatusCode());
        assertEquals("Unexpected response when getting a valid remote application list.", "application/json", response.contentType());

        //Verify server response matches the generated remote application that was previously created WS-TC015
        JsonNode serverReponse = AppManagementAPI.parseGetApplicationResponse(response.body().asString());
        for (AppManagementAPI.AppParameters appParameter : AppManagementAPI.AppParameters.values()) {
            if (remoteAppParameterMap.containsKey(appParameter.getParameter())) {
                String parameterMapValue = remoteAppParameterMap.get(appParameter.getParameter()).toLowerCase();
                String serverResponseValue = serverReponse.findValue(appParameter.getJsonName()).asText().toLowerCase();
                assertEquals("Genterated remote application value " + appParameter.getParameter() + " did not match the server reponse", parameterMapValue, serverResponseValue);
            }
        }

        //Attempt to get a list of applications attached to this user with an invalid token in the URL WS-TC011
        getAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(invalidUserParametersMap, AppManagementAPI.APPLICATIONS_ENDPOINT);
        response = given().contentType(ContentType.URLENC).when().get(getAppEndpoint);
        assertEquals("Unexpected status code when getting remote application list with an invalid user token.", 401, response.getStatusCode());

        //Attempt to create a remote application with a duplicate name
        response = given().contentType(ContentType.URLENC).formParams(negativeRemoteParamterMap).when().post(remoteAppEndpoint);
        assertEquals("Unexpected status code when adding a remote application with an duplicate application name.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a remote application with a duplicate application name.", AppManagementAPI.generateDuplicateRemoteAppNameError(negativeRemoteParamterMap),
                response.asString());

        //Attempt to create a remote application with an invalid device type
        negativeRemoteParamterMap = AppManagementAPI.getInvalidDeviceRemoteAppParameterMap("invalid device type");
        response = given().contentType(ContentType.URLENC).formParams(negativeRemoteParamterMap).when().post(remoteAppEndpoint);
        assertEquals("Unexpected status code when adding a remote application with an invalid device type.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a remote application with an invalid device type.", AppManagementAPI.generateInvalidDeviceError(negativeRemoteParamterMap),
                response.asString());

        //Attempt to create a remote application with an invalid application name
        negativeRemoteParamterMap = AppManagementAPI.getInvalidAppNameRemoteAppParameterMap("@ invalid application name #");
        response = given().contentType(ContentType.URLENC).formParams(negativeRemoteParamterMap).when().post(remoteAppEndpoint);
        assertEquals("Unexpected status code when adding a remote application with an invalid application name.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a remote application with an invalid application name.", AppManagementAPI.INVALID_APP_NAME_ERROR, response.asString());

        //Attempt to create a remote application with a empty string device type
        negativeRemoteParamterMap = AppManagementAPI.getInvalidDeviceRemoteAppParameterMap("");
        response = given().contentType(ContentType.URLENC).formParams(negativeRemoteParamterMap).when().post(remoteAppEndpoint);
        assertEquals("Unexpected status code when adding a remote application with an empty string device type.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a remote application with an empty string device type.", AppManagementAPI.BLANK_DEVICE_TYPE_ERROR, response.asString());

        //Attempt to create a remote application with a empty string application name
        negativeRemoteParamterMap = AppManagementAPI.getInvalidAppNameRemoteAppParameterMap("");
        response = given().contentType(ContentType.URLENC).formParams(negativeRemoteParamterMap).when().post(remoteAppEndpoint);
        assertEquals("Unexpected status code when adding a remote application with an empty string application name.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a remote application with an an empty string application name.", AppManagementAPI.BLANK_APP_NAME_ERROR, response.asString());

        //Attempt to create a remote application with a null device type - Null checks are not possible in manual testing.
        negativeRemoteParamterMap = AppManagementAPI.getInvalidDeviceRemoteAppParameterMap(null);
        response = given().contentType(ContentType.URLENC).formParams(negativeRemoteParamterMap).when().post(remoteAppEndpoint);
        assertEquals("Unexpected status code when adding a remote application with an null device type.", 409, response.getStatusCode());
        //TODO Investigate why RestAssured produces different error messages than direct calls through REST Console
        //assertEquals("Unexpected error message when adding a remote application with an null device type.", AppManagementAPI.BLANK_DEVICE_TYPE_ERROR, response.asString());

        //Attempt to create a remote application with a null application name - Null checks are not possible in manual testing.
        negativeRemoteParamterMap = AppManagementAPI.getInvalidAppNameRemoteAppParameterMap(null);
        response = given().contentType(ContentType.URLENC).formParams(negativeRemoteParamterMap).when().post(remoteAppEndpoint);
        assertEquals("Unexpected status code when adding a remote application with an null application name.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a remote application with an an empty string application name.", AppManagementAPI.BLANK_APP_NAME_ERROR, response.asString());

        //Attempt to create a remote application with a empty string parameter values
        negativeRemoteParamterMap = AppManagementAPI.getBlankRemoteAppParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(negativeRemoteParamterMap).when().post(remoteAppEndpoint);
        assertEquals("Unexpected status code when adding a remote application with empty strings for parameter values.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when adding a remote application with empty strings for parameter values.", AppManagementAPI.BLANK_APP_NAME_ERROR, response.asString());

        //Test the get parameters REST call
        String id = remoteAppParameterMap.get(AppManagementAPI.AppParameters.ID.getParameter());
        String appParamsEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.APPLICATIONS_PARAMETERS_ENDPOINT, id);
        response = given().contentType(ContentType.URLENC).when().get(appParamsEndpoint);
        assertEquals("Unexpected status code when getting valid remote application params.", 200, response.getStatusCode());
        assertEquals("Unexpected response when adding a valid remote application params.", "application/json", response.contentType());

        //Update the remote application and verify WS-TC072
        String updateremoteAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.REMOTE_APPLICATIONS_ENDPOINT, id);
        Map<String, String> updatedremoteAppParameterMap = AppManagementAPI.getHappyUpdatedRemoteAppParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(updatedremoteAppParameterMap).when().put(updateremoteAppEndpoint);
        assertEquals("Unexpected status code when updating a valid remote application.", 204, response.getStatusCode());

        //Attempt to update the remote application with an empty string for an application name
        negativeRemoteParamterMap = AppManagementAPI.getBlankUpdatedRemoteAppParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(negativeRemoteParamterMap).when().put(updateremoteAppEndpoint);
        assertEquals("Unexpected status code when updating a remote application with an empty string application name.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when updating a remote application with an empty string application name.", AppManagementAPI.BLANK_APP_NAME_ERROR, response.asString());

        //Test application delete WS-TC017
        String deleteAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.DELETE_APPLICATIONS_ENDPOINT, id);
        response = given().contentType(ContentType.URLENC).when().delete(deleteAppEndpoint);
        assertEquals("Unexpected status code when deleting a valid remote application.", 204, response.getStatusCode());

        //Attempt to delete an application with an invalid application ID
        deleteAppEndpoint = AppManagementAPI.generateApplicationsEndpoint(userParametersMap, AppManagementAPI.DELETE_APPLICATIONS_ENDPOINT, invalidId);
        response = given().contentType(ContentType.URLENC).when().delete(deleteAppEndpoint);
        assertEquals("Unexpected status code when deleting a remote application with an invalid application id.", 404, response.getStatusCode());
        assertEquals("Unexpected error message when deleting a remote application with an invalid application id.", AppManagementAPI.generateInvalidAppIdError(invalidId), response.asString());
    }
}
