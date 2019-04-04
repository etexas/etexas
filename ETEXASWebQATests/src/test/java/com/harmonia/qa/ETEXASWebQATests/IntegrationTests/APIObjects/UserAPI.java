package com.harmonia.qa.ETEXASWebQATests.IntegrationTests.APIObjects;

import java.util.HashMap;
import java.util.Map;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasProperties;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.RestApiITPage;

/**
 * Container for logic used in integration tests for eTEXAS URLs /user/register
 * AND /user/login
 *
 * @author abishop
 */
public class UserAPI extends RestApiITPage {

    /**
     * User registration URL
     */
    public static final String USER_REGISTRATION_ENDPOINT = "http://" + ETexasProperties.hostName + "/rest/users";

    /**
     * User login URL
     */
    public static final String USER_LOGIN_ENDPOINT = "http://" + ETexasProperties.hostName + "/rest/users/login";

    /**
     * Username creation parameter
     */
    public static final String USERNAME = "username";

    /**
     * Password creation parameter
     */
    public static final String PASSWORD = "password";

    /**
     * Email parameter
     */
    public static final String EMAIL = "email";

    /**
     * First name parameter
     */
    public static final String FIRST_NAME = "firstName";

    /**
     * Last name parameter
     */
    public static final String LAST_NAME = "lastName";

    /**
     * Organization parameter
     */
    public static final String ORGANIZATION = "organization";

    /**
     * Organization parameter
     */
    public static final String TOKEN = "token";

    /**
     * Error message for an invalid email format
     */
    public static final String EMAIL_ERROR_FORMAT = "Invalid Email Address;Email addresses must be in the proper format.";

    /**
     * Error message for an invalid email
     */
    public static final String EMAIL_ERROR_INVALID = "Invalid Email Address;A valid email address is required.";

    /**
     * Error message for an invalid email format
     */
    public static final String EMAIL_ERROR_LENGTH = "Invalid Email Address;Email addresses may not exceed 254 characters.";

    /**
     * Error message for an invalid password
     */
    public static final String PASSWORD_ERROR_INVALID = "Invalid Password;A valid password is required.";

    /**
     * Error message for a first name using invalid characters
     */
    public static final String FIRST_NAME_ERROR_CHARACTER = "Invalid First Name;First names may contain only letters, spaces, hyphens, periods, commas, and apostrophes.";

    /**
     * Error message for an invalid first name
     */
    public static final String FIRST_NAME_ERROR_INVALID = "Invalid First Name;A valid first name is required.";

    /**
     * Error message for a first name containing leading or trailing spaces
     */
    public static final String FIRST_NAME_ERROR_WHITESPACE = "Invalid First Name;First names may not contain leading or trailing spaces.";

    /**
     * Error message for a last name containing invalid characters
     */
    public static final String LAST_NAME_ERROR_CHARACTER = "Invalid Last Name;Last names may contain only letters, spaces, hyphens, periods, commas, and apostrophes.";

    /**
     * Error message for an invalid last name
     */
    public static final String LAST_NAME_ERROR_INVALID = "Invalid Last Name;A valid last name is required.";

    /**
     * Error message for a last name containing leading or trailing whitespace
     */
    public static final String LAST_NAME_ERROR_WHITESPACE = "Invalid Last Name;Last names may not contain leading or trailing spaces.";

    /**
     * Error message for an organization name containing invalid characters
     */
    public static final String ORGANIZATION_ERROR_CHARACTER = "Invalid Organization;Organization names may contain only letters, digits, spaces, hyphens, periods, commas, and apostrophes.";

    /**
     * Error message for an organization name containing leading or trailing
     * whitespace
     */
    public static final String ORGANIZATION_ERROR_WHITESPACE = "Invalid Organization;Organization names may not contain leading or trailing spaces.";

    /**
     * Error message for an invalid username containing leading or trailing
     * whitespace
     */
    public static final String USERNAME_ERROR_WHITESPACE = "Invalid Username;Usernames may not contain leading or trailing spaces.";

    /**
     * Error message for an invalid username
     */
    public static final String USERNAME_ERROR_INVALID = "Invalid Username;A valid username is required.";

    /**
     * Error message for an invalid username containing invalid character(s)
     */
    public static final String USERNAME_ERROR_CHARACTER = "Invalid Username;Usernames must start with a valid letter and may contain only letters and digits.";

    /**
     * Generates a duplicate user name error message
     *
     * @param username used in the POST call
     * @return error message
     */
    public static String generateDuplicateUserNameError(String username) {
        return "Add User Failure;A user with the username \"" + username + "\" already exists.";
    }

    /**
     * Generates a duplicate email address error message
     *
     * @param email used in the POST call
     * @return error message
     */
    public static String generateDuplicateEmailError(String email) {
        return "Add User Failure;A user with the email \"" + email + "\" already exists.";
    }

    /**
     * Generate a parameter map containing values for an happy eTexas user
     *
     * @return userParameter HashMap RestAssured digests Map one entry (<String,
     *         String>) at a time. The first string represents the parameter
     *         name, the second string represents the parameter value. It reads
     *         the entries procedurally, so it is important to maintain the
     *         order expected by the API being used
     */
    public static Map<String, String> getHappyUserParameterMap() {

        //Stores the parameters used for API POST calls
        Map<String, String> userParametersMap = new HashMap<>();

        //Generate a random user
        ETexasUser user = ETexasUserFactory.getUser(true);

        //eTEXAS validator enforces the name to start with a char and be followed by a series of chars and/or numbers
        userParametersMap.put(USERNAME, user.getUsername());
        userParametersMap.put(PASSWORD, user.getPassword());
        userParametersMap.put(EMAIL, user.getEmailAddress());
        userParametersMap.put(FIRST_NAME, user.getFirstName());
        userParametersMap.put(LAST_NAME, user.getLastName());
        userParametersMap.put(ORGANIZATION, user.getOrganization());

        return userParametersMap;
    }

    /**
     * Generate a parameter map containing values for the given eTexas user
     *
     * @param user -the existing user to create a user parameter map for
     * @return userParameter HashMap RestAssured digests Map one entry (<String,
     *         String>) at a time. The first string represents the parameter
     *         name, the second string represents the parameter value. It reads
     *         the entries procedurally, so it is important to maintain the
     *         order expected by the API being used
     */
    public static Map<String, String> getUserParameterMap(ETexasUser user) {

        //Stores the parameters used for API POST calls
        Map<String, String> userParametersMap = new HashMap<>();

        //eTEXAS validator enforces the name to start with a char and be followed by a series of chars and/or numbers
        userParametersMap.put(USERNAME, user.getUsername());
        userParametersMap.put(PASSWORD, user.getPassword());
        userParametersMap.put(EMAIL, user.getEmailAddress());
        userParametersMap.put(FIRST_NAME, user.getFirstName());
        userParametersMap.put(LAST_NAME, user.getLastName());
        userParametersMap.put(ORGANIZATION, user.getOrganization());
        return userParametersMap;
    }

    /**
     * Generate a parameter map containing values for default admin user
     *
     * @return userParameter HashMap RestAssured digests Map one entry (<String,
     *         String>) at a time. The first string represents the parameter
     *         name, the second string represents the parameter value. It reads
     *         the entries procedurally, so it is important to maintain the
     *         order expected by the API being used
     */
    public static Map<String, String> getAdminParametersMap() {
        //Stores the parameters for admin used for API Admin calls
        Map<String, String> userParametersMap = new HashMap<>();
        userParametersMap.put(USERNAME, "admin");
        userParametersMap.put(PASSWORD, "password");
        return userParametersMap;
    }

    /**
     * Generates a fully qualified admin verify REST API URL containing a logged
     * in username and their respective session token
     *
     * @param adminParametersMap Logged in admin user
     * @return URL for Admin API calls for the given user
     */
    public static String generateAdminVerifyURL(Map<String, String> adminParametersMap) {
        String simulationEndpoint = "http://" + adminParametersMap.get(USERNAME) + ":" + adminParametersMap.get(TOKEN) + "@" + ETexasProperties.hostName + "/rest/admin/verify";
        return simulationEndpoint;
    }

    /**
     * Captures a session token after a user has been successfully logged in
     *
     * @param userParametersMap the login parameter map that the token will be
     *        attached to
     * @param token the session token included in the server response
     * @return updated login paramter map
     */
    public static Map<String, String> captureUserToken(Map<String, String> userParametersMap, String token) {
        userParametersMap.put(TOKEN, token);
        return userParametersMap;
    }

    /**
     * Generates an ETexasUser from a HashMap containing valid user values and
     * parameters
     *
     * @param userParametersMap the parameter map with the user vlaues that are
     *        to be copied into an ETexasUser
     * @return ETexasUser copied from the given HashMap
     */
    public static ETexasUser getUserFromHashMap(Map<String, String> userParametersMap) {
        ETexasUser user = new ETexasUser();
        user.setEmailAddress(userParametersMap.get(UserAPI.EMAIL));
        user.setFirstName(userParametersMap.get(UserAPI.FIRST_NAME));
        user.setLastName(userParametersMap.get(UserAPI.LAST_NAME));
        user.setOrganization(userParametersMap.get(UserAPI.ORGANIZATION));
        user.setPassword(userParametersMap.get(UserAPI.PASSWORD));
        user.setUsername(userParametersMap.get(UserAPI.USERNAME));
        if (userParametersMap.containsKey(TOKEN)) {
            user.setToken(userParametersMap.get(TOKEN));
        }
        return user;
    }
}
