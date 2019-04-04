package com.harmonia.qa.ETEXASWebQATests.entities.factories;

import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Entity factory for generating RSE device objects
 *
 * @author llaroussini
 */
public class RSEDeviceFactory {

    /**
     * Returns a new instance of the RSE device object
     *
     * @return a new RSE device object
     */
    public static RSEDevice getRSEDevice() {
        return instantiateRSEDevice();
    }

    /**
     * Gets a new RSE device which may be random or static
     *
     * @param random whether or not the returned device should have static or
     *        randomized values assigned
     * @return the newly created RSE device according to the randomization
     *         parameter
     */
    public static RSEDevice getRSEDevice(boolean random) {
        if (random) {
            return getRandomRSEDevice();
        }
        else {
            return getStaticRSEDevice();
        }
    }

    /**
     * Gets an RSE device with randomly assigned values
     *
     * @return a new RSE device
     */
    private static RSEDevice getRandomRSEDevice() {
        RSEDevice rseDevice = instantiateRSEDevice();
        rseDevice.setName(RandomStringGenerator.nextLetterString(15));
        rseDevice.setXCoordinate(Integer.toString(RandomNumberGenerator.nextInteger()));
        rseDevice.setYCoordinate(Integer.toString(RandomNumberGenerator.nextInteger()));
        rseDevice.setZCoordinate(Integer.toString(RandomNumberGenerator.nextInteger()));
        return rseDevice;
    }

    /**
     * Gets an RSE device with a known set of values
     *
     * @return a new RSE device with known values
     */
    private static RSEDevice getStaticRSEDevice() {
        RSEDevice rseDevice = instantiateRSEDevice();
        rseDevice.setName("Test RSE");
        rseDevice.setXCoordinate("10");
        rseDevice.setYCoordinate("20");
        rseDevice.setZCoordinate("50");
        return rseDevice;
    }

    /**
     * Default instantiation
     *
     * @return a newly constructed RSE device object
     */
    private static RSEDevice instantiateRSEDevice() {
        return new RSEDevice();
    }

}
