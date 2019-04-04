package com.harmonia.qa.ETEXASWebQATests.entities.factories;

import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Entity factory for generating OBU device objects
 *
 * @author llaroussini
 */
public class OBUDeviceFactory {

    /**
     * Returns a new instance of the OBU device
     *
     * @return a new OBU device object
     */
    public static OBUDevice getOBUDevice() {
        return instantiateOBUDevice();
    }

    /**
     * Gets a new OBU device which may be returned random or static
     *
     * @param random whether or not the returned device should have static or
     *        randomized values assigned
     * @return the newly created OBU device according to the randomization
     *         parameter
     */
    public static OBUDevice getOBUDevice(boolean random) {
        if (random) {
            return getRandomOBUDevice();
        }
        else {
            return getStaticOBUDevice();
        }
    }

    /**
     * Gets an OBU device with randomly assigned values
     *
     * @return a new OBU device
     */
    private static OBUDevice getRandomOBUDevice() {
        OBUDevice obuDevice = instantiateOBUDevice();
        obuDevice.setOBUName(RandomStringGenerator.nextLetterString(15));
        obuDevice.setOBUPercent(Integer.toString(RandomNumberGenerator.nextInteger(100)));
        return obuDevice;
    }

    /**
     * Gets an OBU device with a known set of values
     *
     * @return a new OBU Device with known values
     */
    private static OBUDevice getStaticOBUDevice() {
        OBUDevice obuDevice = instantiateOBUDevice();
        obuDevice.setOBUName("OBU-100");
        obuDevice.setOBUPercent("100");
        return obuDevice;
    }

    /**
     * Default instantiation
     *
     * @return a newly constructed OBU device object
     */
    private static OBUDevice instantiateOBUDevice() {
        return new OBUDevice();
    }
}
