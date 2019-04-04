package com.harmonia.qa.ETEXASWebQATests.entities.factories;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Entity factory for generating user objects
 *
 * @author cbulloss
 */
public class ETexasUserFactory {

    /**
     * Prevent public construction
     */
    private ETexasUserFactory() {
    }

    /**
     * Returns a new instance of the User object
     *
     * @return a new user object
     */
    public static ETexasUser getUser() {
        return instantiateUser();
    }

    /**
     * Gets a new user which may be random or static
     *
     * @param random true for a randomly generated user false for a static user
     * @return the newly created user according to the randomization parameter
     */
    public static ETexasUser getUser(boolean random) {
        if (random) {
            return getRandomUser();
        }
        else {
            return getStaticUser();
        }
    }

    /**
     * Gets a user with randomly assigned values
     *
     * @return
     */
    private static ETexasUser getRandomUser() {
        ETexasUser user = instantiateUser();
        user.setUsername(RandomStringGenerator.nextLetterString(15));
        user.setPassword(RandomStringGenerator.nextString(15, true));
        user.setFirstName(RandomStringGenerator.nextLetterString(15));
        user.setLastName(RandomStringGenerator.nextLetterString(15));
        user.setEmailAddress(RandomStringGenerator.getRandomEmailAddress());
        user.setOrganization(RandomStringGenerator.nextLetterString(15));
        return user;
    }

    /**
     * Gets a user with a known set of values
     *
     * @return a new user with known values
     */
    private static ETexasUser getStaticUser() {
        ETexasUser user = instantiateUser();
        user.setUsername("testuser");
        user.setPassword("password");
        user.setFirstName("Test");
        user.setLastName("User");
        user.setEmailAddress("testuser@harmonia.com");
        user.setOrganization("Harmonia");
        return user;
    }

    /**
     * Default instantiation
     *
     * @return a newly constructed user object
     */
    private static ETexasUser instantiateUser() {
        return new ETexasUser();
    }
}
