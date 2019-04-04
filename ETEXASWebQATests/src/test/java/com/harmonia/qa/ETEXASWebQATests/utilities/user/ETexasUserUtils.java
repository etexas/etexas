package com.harmonia.qa.ETEXASWebQATests.utilities.user;

import static io.restassured.RestAssured.given;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import io.restassured.http.ContentType;
import io.restassured.response.Response;

import java.util.Map;

import com.fasterxml.jackson.databind.JsonNode;
import com.harmonia.qa.ETEXASWebQATests.IntegrationTests.APIObjects.UserAPI;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.NewUserForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.RegisterSuccessWindow;

/**
 * Utility class to assist with managing user objects in the eTexas application
 *
 * @author llaroussini
 * @author abishop
 */
public class ETexasUserUtils extends ETexasCommonUtils {

    /**
     * Performs standard user registration using a given user object
     *
     * @param user The user to be registered
     * @return the landing page displayed at the end of registration
     */
    public static LandingPage userRegistration(ETexasUser user) {
        LandingPage landingPage = ETexasCommonUtils.goToLandingPage();
        NewUserForm userForm = landingPage.clickSignUp();
        userForm.populateForm(user);
        RegisterSuccessWindow successWindow = userForm.clickRegisterBtnRtrnSuccess();
        successWindow.clickOk();
        verifyUser(user);
        return getPage(LandingPage.class);
    }

    /**
     * Sends REST call to verify user as Admin to bypass the full email
     * verification process and verifies status code of 204 received
     *
     * @param user the user to be verified
     */
    public static void verifyUser(ETexasUser user) {

        //Get token for admin user
        Map<String, String> adminParametersMap = UserAPI.getAdminParametersMap();
        Response response = given().contentType(ContentType.URLENC).params(adminParametersMap).when().post(UserAPI.USER_LOGIN_ENDPOINT);
        JsonNode node = parseGetApplicationResponse(response.asString());
        node = node.get("token");
        String token = node.asText();
        UserAPI.captureUserToken(adminParametersMap, token);

        //Verify user
        Map<String, String> userParametersMap = UserAPI.getUserParameterMap(user);
        response = given().contentType(ContentType.URLENC).formParams(userParametersMap).when().put(UserAPI.generateAdminVerifyURL(adminParametersMap));
        assertEquals("Unexpected status code when verifying user with username: " + user.getUsername() + ".", 204, response.getStatusCode());
    }

    /**
     * Many of the REST API calls require a username and token to be appended to
     * the REST URL. This method creates a new user, registers them, verifies
     * them, then finally logs them in, and attaches the session token returned
     * by the server.
     *
     * @return The logged in user complete with session token returned by the
     *         server
     */
    public static ETexasUser getLoggedInUser() {

        //Create a valid user and capture them in an ETexasUser Object and a HashMap for API calls
        ETexasUser user = ETexasUserFactory.getUser(true);
        Map<String, String> userParametersMap = UserAPI.getUserParameterMap(user);

        //Check for OK status
        Response response = given().contentType(ContentType.URLENC).params(userParametersMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when valid registration values used.", 204, response.getStatusCode());

        //Verify the user through the admin REST API
        ETexasUserUtils.verifyUser(user);

        //Get login parameters and attempt to login new user. Then validate response
        response = given().contentType(ContentType.URLENC).params(userParametersMap).when().post(UserAPI.USER_LOGIN_ENDPOINT);
        JsonNode node = parseGetApplicationResponse(response.asString());
        node = node.get("token");
        String token = node.asText();
        assertEquals("Unexpected status code when valid login values used.", 200, response.getStatusCode());
        assertNotNull("Unexpected response string when valid login values used.", response.body());

        user.setToken(token);

        return user;
    }
}
