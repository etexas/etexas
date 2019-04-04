package com.harmonia.qa.ETEXASWebQATests.entities.factories;

import java.io.File;

import com.harmonia.qa.ETEXASWebQATests.entities.UserJarApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserNativeApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserRemoteApp;
import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Entity factory for generating App objects
 *
 * @author llaroussini
 */
public class AppFactory {

    /**
     * Regex expression for generating IPv4 addresses
     */
    public static final String IPV4_REGEX = "((\\d{1}|[1-9]\\d|[1]\\d{2}|[2][0-4]\\d|[2][5][0-5])\\.){3}(\\d{1}|[1-9]\\d|[1]\\d{2}|[2][0-4]\\d|[2][5][0-5])";

    /**
     * Email parameter
     */
    public static final String IPV6_REGEX = "(([0]{1}|[1-9a-fA-F][0-9a-fA-F]{0,3}):){7}([0]{1}|[1-9a-fA-F][0-9a-fA-F]{0,3})";

    /**
     * Gets a new Native App which may be returned random or static
     *
     * @param random true for a randomly generated app false for a static app
     * @return the newly created Native App according to the randomization
     *         parameter
     */
    public static UserNativeApp getUserNativeApp(boolean random) {
        if (random) {
            return getRandomUserNativeApp();
        }
        else {
            return getStaticUserNativeApp();
        }
    }

    /**
     * Gets a User Native App with randomly assigned values
     *
     * @return a new User Native App
     */
    private static UserNativeApp getRandomUserNativeApp() {
        UserNativeApp app = new UserNativeApp();
        app.setName(RandomStringGenerator.nextLetterString(10));
        app.setDeviceType(getRandomDeviceType());
        app.setCommandLine(RandomStringGenerator.nextLetterString(5));
        app.setHost(getRandomIP());
        app.setPort(generateRandomValidPort());
        return app;
    }

    /**
     * Gets a User Native App with a known set of values
     *
     * @return a new User Native App with known values
     */
    private static UserNativeApp getStaticUserNativeApp() {
        UserNativeApp app = new UserNativeApp();
        app.setName("New Native Report App");
        app.setDeviceType(DeviceType.REPORT);
        app.setCommandLine("commandme -O option");
        app.setHost("104.159.53.158");
        app.setPort("4040");
        return app;
    }

    /**
     * Gets a new Remote App which may be returned random or static
     *
     * @param random true for a random app, false for a static app
     * @return the newly created remote app according to the randomization
     *         parameter
     */
    public static UserRemoteApp getUserRemoteApp(boolean random) {
        if (random) {
            return getRandomUserRemoteApp();
        }
        else {
            return getStaticUserRemoteApp();
        }
    }

    /**
     * Gets a User Remote App with randomly assigned values
     *
     * @return a new User Remote App
     */
    private static UserRemoteApp getRandomUserRemoteApp() {
        UserRemoteApp app = new UserRemoteApp();
        app.setName(RandomStringGenerator.nextLetterString(10));
        app.setDeviceType(getRandomDeviceType());
        return app;
    }

    /**
     * Gets a User Remote App with a known set of values
     *
     * @return a new User Remote App with known values
     */
    private static UserRemoteApp getStaticUserRemoteApp() {
        UserRemoteApp app = new UserRemoteApp();
        app.setName("New Remote OBU App");
        app.setDeviceType(DeviceType.OBU);
        return app;
    }

    /**
     * Gets a new Jar App
     *
     * @param file -jar file for app
     * @param type -the device type associated with the file being uploaded
     * @param name -the name to assign to the app
     * @return the newly created remote app
     */
    public static UserJarApp getUserJarApp(String name, DeviceType type, File file) {
        UserJarApp app = new UserJarApp();
        app.setName(name);
        app.setDeviceType(type);
        app.addFile(file);
        return app;
    }

    /**
     * Generates a random DeviceType based on the enum
     *
     * @return the randomly chosen value from DeviceType
     */
    public static DeviceType getRandomDeviceType() {
        DeviceType[] appTypes = DeviceType.values();
        return appTypes[RandomNumberGenerator.nextInteger(appTypes.length - 1)];
    }

    /**
     * Generates a string representation of a valid port value for an app. The
     * valid port range is 1-65535
     *
     * @return port value as a String
     */
    public static String generateRandomValidPort() {
        return String.valueOf(RandomNumberGenerator.nextInteger(65535) + 1);
    }

    /**
     * Creates a randomly generated IP address of a randomly chosen type (IPv4
     * or IPv6)
     *
     * @return String value of the randomly generated IP address
     */
    public static String getRandomIP() {
        int random = RandomNumberGenerator.nextInteger(2);
        String ipAddress = null;
        if (random == 0) {
            ipAddress = RandomStringGenerator.generateStringFromRegex(IPV4_REGEX);
        }
        else {
            ipAddress = RandomStringGenerator.generateStringFromRegex(IPV6_REGEX);
        }
        return ipAddress;
    }
}
