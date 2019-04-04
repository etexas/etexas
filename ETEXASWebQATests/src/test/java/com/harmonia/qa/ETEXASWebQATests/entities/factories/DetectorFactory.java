package com.harmonia.qa.ETEXASWebQATests.entities.factories;

import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneID;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;

/**
 * Entity factory for generating detector objects
 *
 * @author llaroussini
 */
public class DetectorFactory {

    /**
     * Returns a new instance of the detector
     *
     * @return a new detector object
     */
    public static Detector getDetector() {
        return instantiateDetector();
    }

    /**
     * Gets a new detector which may be returned random or static
     *
     * @param random whether or not the returned detector should have static or
     *        randomized values assigned
     * @return the newly created detector according to the randomization
     *         parameter
     */
    public static Detector getDetector(boolean random) {
        if (random) {
            return getRandomDetector();
        }
        else {
            return getStaticDetector();
        }
    }

    /**
     * Gets a detector with randomly assigned values
     *
     * @return a new detector
     */
    private static Detector getRandomDetector() {
        Detector detector = instantiateDetector();
        LaneID[] lanes = LaneID.values();
        LaneID lane = lanes[RandomNumberGenerator.nextInteger(lanes.length)];
        detector.setLane(ETexasEntityManager.getLane(lane));
        detector.setWidth(Integer.toString(RandomNumberGenerator.nextInteger(400)));
        detector.setHeight(Integer.toString(RandomNumberGenerator.nextInteger(400)));
        detector.setDistance(Integer.toString(RandomNumberGenerator.nextInteger(4000)));
        return detector;
    }

    /**
     * Gets a detector with a known set of values
     *
     * @return a new detector with known values
     */
    private static Detector getStaticDetector() {
        Detector detector = instantiateDetector();
        detector.setLane(ETexasEntityManager.getLane(LaneID.ONE));
        detector.setWidth("100");
        detector.setHeight("50");
        detector.setDistance("500");
        return detector;
    }

    /**
     * Default instantiation
     *
     * @return a newly constructed detector object
     */
    private static Detector instantiateDetector() {
        return new Detector();
    }
}
