package com.harmonia.qa.ETEXASWebQATests.IntegrationTests;

import static io.restassured.RestAssured.given;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import io.restassured.http.ContentType;
import io.restassured.response.Response;

import java.util.Map;

import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.IntegrationTests.APIObjects.UserAPI;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * eTexas REST API Integration Tests IDs:(WS-TC042, WS-TC043)
 *
 * @author abishop
 */
public class UserServicesIT extends ETexasAfterTestResetTestBase {

    /**
     * Map containing parameter values for registering/logging in an eTEXAS user
     */
    Map<String, String> userParameterMap;

    /**
     * Map containing parameter values for registering/logging in an eTEXAS user
     * when testing duplicate values
     */
    Map<String, String> userParameterMap2;

    /**
     * Covers section 5 (WS-TC042, WS-TC043) of the eTEXAS Rest API test plan
     */
    @Test
    public void userRegistrationIT() {

        //Stores the parameters used for API POST calls with happy path user
        userParameterMap = UserAPI.getHappyUserParameterMap();

        //Check for OK status and a JSON formatted response containing a username
        Response response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when valid registration values used.", 200, response.getStatusCode());
        assertEquals("Unexpected response string when valid registration values used.", userParameterMap.get(UserAPI.USERNAME), response.body().asString());

        ///////////////////
        /// userName Tests
        ///////////////////

        //Update map with an invalid an empty string username and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.USERNAME, "");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when an empty String username was used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when an empty String username was used.", UserAPI.USERNAME_ERROR_INVALID, response.asString());

        //Update map with an invalid username containing whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.USERNAME, "  ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when username containing white space used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when username containing white space used.", UserAPI.USERNAME_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid username containing leading whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.USERNAME, " username");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when username containing leading whitespace used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when username containing leading whitespace used.", UserAPI.USERNAME_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid username containing trailing whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.USERNAME, "username ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when username containing trailing whitespace used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when username containing trailing whitespace used.", UserAPI.USERNAME_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid username containing invalid characters and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.USERNAME, "&%^*");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when username containing special characters used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when username containing special chararacters used.", UserAPI.USERNAME_ERROR_CHARACTER, response.asString());

        //Update map with an invalid username containing an invalid leading character and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.USERNAME, "2username");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when username containing special characters used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when username containing special chararacters used.", UserAPI.USERNAME_ERROR_CHARACTER, response.asString());

        //Update map by removing the username parameter and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.remove(UserAPI.USERNAME);
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when no username parameter was passed.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when no username parameter was passed.", UserAPI.USERNAME_ERROR_INVALID, response.asString());

        //Register a new valid user and verify a 200 OK status code
        userParameterMap = UserAPI.getHappyUserParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when valid registration values used.", 200, response.getStatusCode());

        //Attempt to register a user with the same username and verify response
        userParameterMap2 = UserAPI.getHappyUserParameterMap();
        userParameterMap2.put(UserAPI.USERNAME, userParameterMap.get(UserAPI.USERNAME));
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap2).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when duplicate username used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when duplicate username used.", UserAPI.generateDuplicateUserNameError(userParameterMap2.get(UserAPI.USERNAME)), response.asString());

        ///////////////////
        /// password Tests
        ///////////////////

        //Update map with an invalid password containing an empty string and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.PASSWORD, "");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when an empty String password was used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when an empty String password was used.", UserAPI.PASSWORD_ERROR_INVALID, response.asString());

        //Update map with an invalid password containing whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.PASSWORD, "  ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when password containing white space used.", 200, response.getStatusCode());

        //Update map with an invalid password containing leading whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.PASSWORD, " password");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when password containing leading whitespace used.", 200, response.getStatusCode());

        //Update map with an invalid password containing trailing whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.PASSWORD, "password ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when password containing trailing whitespace used.", 200, response.getStatusCode());

        //Update map with an invalid password containing consecutive whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.PASSWORD, "pass  word");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when password containing trailing whitespace used.", 200, response.getStatusCode());

        //Update map with an invalid password containing special characters and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.PASSWORD, "&%^*");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when password containing special characters used.", 200, response.getStatusCode());

        //Update map by removing the password parameter and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.remove(UserAPI.PASSWORD);
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when no password parameter was passed.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when no password parameter was passed.", UserAPI.PASSWORD_ERROR_INVALID, response.asString());

        ///////////////////
        /// email Tests
        ///////////////////

        //Update map with an empty String for an email address and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.EMAIL, "");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when invalid email format used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when invalid email format used.", UserAPI.EMAIL_ERROR_INVALID, response.asString());

        //Update map with an invalid containing only whitespace email address and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.EMAIL, "   ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when invalid email format used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when invalid email format used.", UserAPI.EMAIL_ERROR_FORMAT, response.asString());

        //Update map with an invalidly formatted email address and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.EMAIL, "invalidEmailFormat");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when invalid email format used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when invalid email format used.", UserAPI.EMAIL_ERROR_FORMAT, response.asString());

        //Update map with an invalid email address that is over 254 characters and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.EMAIL, RandomStringGenerator.nextLetterString(256));
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when invalid email format used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when invalid email format used.", UserAPI.EMAIL_ERROR_LENGTH, response.asString());

        //Update map with by removing the email parameter and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.remove(UserAPI.EMAIL);
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when invalid email format used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when invalid email format used.", UserAPI.EMAIL_ERROR_INVALID, response.asString());

        //Register a new happy user and verify a 200 OK status code
        userParameterMap = UserAPI.getHappyUserParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when valid registration values used.", 200, response.getStatusCode());
        assertEquals("Unexpected response string when valid registration values used.", userParameterMap.get(UserAPI.USERNAME), response.body().asString());

        //Update map and attempt to register another user with the same email address
        userParameterMap2 = UserAPI.getHappyUserParameterMap();
        userParameterMap2.put(UserAPI.EMAIL, userParameterMap.get(UserAPI.EMAIL));
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap2).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when duplicate email used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when duplicate email used.", UserAPI.generateDuplicateEmailError(userParameterMap2.get(UserAPI.EMAIL)), response.asString());

        ///////////////////
        /// firstName Tests
        ///////////////////

        //Update map with an invalid an empty String for a first name and verify the response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.FIRST_NAME, "");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when an empty String first name was used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when an empty String first name was used.", UserAPI.FIRST_NAME_ERROR_INVALID, response.asString());

        //Update map with an invalid first name containing whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.FIRST_NAME, "  ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when first name containing white space used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when first name containing white space used.", UserAPI.FIRST_NAME_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid first name containing leading whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.FIRST_NAME, " firstname");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when first name containing leading whitespace used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when first name containing leading whitespace used.", UserAPI.FIRST_NAME_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid first name containing trailing whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.FIRST_NAME, "firstname ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when first name containing trailing whitespace used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when first name containing trailing whitespace used.", UserAPI.FIRST_NAME_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid first name containing invalid characters and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.FIRST_NAME, "&%^*");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when first name containing special characters used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when first name containing special chararacters used.", UserAPI.FIRST_NAME_ERROR_CHARACTER, response.asString());

        //Update map by removing the first name parameter and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.remove(UserAPI.FIRST_NAME);
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when no first name parameter was passed.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when no first name parameter was passed.", UserAPI.FIRST_NAME_ERROR_INVALID, response.asString());

        ///////////////////
        /// lastName Tests
        ///////////////////

        //Update map with an invalid an empty String for a last name and verify the response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.LAST_NAME, "");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when an empty String last name was used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when an empty String last name was used.", UserAPI.LAST_NAME_ERROR_INVALID, response.asString());

        //Update map with an invalid last name containing whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.LAST_NAME, "  ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when last name containing white space used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when last name containing white space used.", UserAPI.LAST_NAME_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid last name containing leading whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.LAST_NAME, " lastname");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when last name containing leading whitespace used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when last name containing leading whitespace used.", UserAPI.LAST_NAME_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid last name containing trailing whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.LAST_NAME, "lastname ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when last name containing trailing whitespace used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when last name containing trailing whitespace used.", UserAPI.LAST_NAME_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid last name containing invalid characters and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.LAST_NAME, "&%^*");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when last name containing special characters used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when last name containing special chararacters used.", UserAPI.LAST_NAME_ERROR_CHARACTER, response.asString());

        //Update map by removing the last name parameter and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.remove(UserAPI.LAST_NAME);
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when no last name parameter was passed.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when no last name parameter was passed.", UserAPI.LAST_NAME_ERROR_INVALID, response.asString());

        ///////////////////////
        /// organization Tests
        ///////////////////////

        //Update map with an invalid an empty String for a organization and verify the response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.ORGANIZATION, "");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when an empty String organization was used.", 200, response.getStatusCode());

        //Update map with an invalid organization containing whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.ORGANIZATION, "  ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when organization containing white space used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when organization containing white space used.", UserAPI.ORGANIZATION_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid organization containing leading whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.ORGANIZATION, " firstname");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when organization containing leading whitespace used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when organization containing leading whitespace used.", UserAPI.ORGANIZATION_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid organization containing trailing whitespace and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.ORGANIZATION, "firstname ");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when organization containing trailing whitespace used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when organization containing trailing whitespace used.", UserAPI.ORGANIZATION_ERROR_WHITESPACE, response.asString());

        //Update map with an invalid organization containing invalid characters and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.put(UserAPI.ORGANIZATION, "&%^*");
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when organization containing special characters used.", 409, response.getStatusCode());
        assertEquals("Unexpected error message when organization containing special chararacters used.", UserAPI.ORGANIZATION_ERROR_CHARACTER, response.asString());

        //Update map by removing the organization parameter and verify response
        userParameterMap = UserAPI.getHappyUserParameterMap();
        userParameterMap.remove(UserAPI.ORGANIZATION);
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when no organization parameter was passed.", 200, response.getStatusCode());

        ///////////////////
        /// login Tests
        ///////////////////

        //Register a new user and check for OK status and a JSON formatted response containing a username
        userParameterMap = UserAPI.getHappyUserParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_REGISTRATION_ENDPOINT);
        assertEquals("Unexpected status code when valid registration values used.", 200, response.getStatusCode());
        assertEquals("Unexpected response string when valid registration values used.", userParameterMap.get(UserAPI.USERNAME), response.body().asString());
        ETexasUserUtils.verifyUser(UserAPI.getUserFromHashMap(userParameterMap));

        //Get login parameters and attempt to login new user. Then validate response
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_LOGIN_ENDPOINT);
        assertEquals("Unexpected status code when valid login values used.", 200, response.getStatusCode());
        assertNotNull("Unexpected response string when valid login values used.", response.body());

        //Attempt to login in the user that is already logged in
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_LOGIN_ENDPOINT);
        assertEquals("Unexpected status code when valid login values used consecutively.", 200, response.getStatusCode());
        assertNotNull("Unexpected response string when valid login values used consecutively.", response.body());

        //Generate a user with an invalid username, and attempt to login
        userParameterMap = UserAPI.getHappyUserParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_LOGIN_ENDPOINT);
        assertEquals("Unexpected status code when invalid username values used.", 404, response.getStatusCode());

        //Generate a user with an invalid password and attempt to login
        userParameterMap = UserAPI.getHappyUserParameterMap();
        response = given().contentType(ContentType.URLENC).formParams(userParameterMap).when().post(UserAPI.USER_LOGIN_ENDPOINT);
        assertEquals("Unexpected status code when invalid password values used.", 404, response.getStatusCode());
    }
}
