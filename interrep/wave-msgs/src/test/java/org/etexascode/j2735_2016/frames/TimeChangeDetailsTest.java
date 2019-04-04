/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.j2735_2016.frames;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.etexascode.j2735_2016.elements.TimeIntervalConfidence;
import org.etexascode.j2735_2016.elements.TimeMark;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the time change details frame.
 * 
 * @author ttevendale
 */
public class TimeChangeDetailsTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    TimeChangeDetails timing;

    @Before
    public void init() {

        TimeMark startTime = new TimeMark(39);
        TimeMark minEndTime = new TimeMark(20);
        TimeMark maxEndTime = new TimeMark(40);
        TimeMark likelyTime = new TimeMark(39);
        TimeIntervalConfidence confidence = new TimeIntervalConfidence(5);
        TimeMark nextTime = new TimeMark(100);

        timing = new TimeChangeDetails(minEndTime);
        timing.setStartTime(startTime);
        timing.setMaxEndTime(maxEndTime);
        timing.setLikelyTime(likelyTime);
        timing.setConfidence(confidence);
        timing.setNextTime(nextTime);
    }

    @Test
    public void testConstructor() {

        TimeMark minEndTime = new TimeMark(100);

        TimeChangeDetails timing = new TimeChangeDetails(minEndTime);

        assertNull(timing.getStartTime());
        assertNull(timing.getMaxEndTime());
        assertNull(timing.getLikelyTime());
        assertNull(timing.getConfidence());
        assertNull(timing.getNextTime());
        assertTrue(minEndTime.equals(timing.getMinEndTime()));

        thrown.expect(NullPointerException.class);
        new TimeChangeDetails(null);
    }

    @Test
    public void testConstructorPrimitive() {

        int minEndTime = 121;

        TimeChangeDetails timing = new TimeChangeDetails(minEndTime);

        assertTrue(minEndTime == timing.getMinEndTime().getValue());
    }

    @Test
    public void testSetStartTimePrimitive() {

        int startTime = 84;

        TimeChangeDetails timing = new TimeChangeDetails();
        timing.setStartTime(startTime);

        assertTrue(startTime == timing.getStartTime().getValue());

        startTime = 12;

        timing.setStartTime(startTime);

        assertTrue(startTime == timing.getStartTime().getValue());
    }

    @Test
    public void testSetMinEndTime() {

        TimeMark minEndTime = new TimeMark(12);

        TimeChangeDetails timing = new TimeChangeDetails();
        timing.setMinEndTime(minEndTime);

        assertTrue(minEndTime.equals(timing.getMinEndTime()));

        thrown.expect(NullPointerException.class);
        timing.setMinEndTime(null);
    }

    @Test
    public void testSetMinEndTimePrimitive() {

        int minEndTime = 1000;

        TimeChangeDetails timing = new TimeChangeDetails();
        timing.setMinEndTime(minEndTime);

        assertTrue(minEndTime == timing.getMinEndTime().getValue());
    }

    @Test
    public void testSetMaxEndTimePrimitive() {

        int maxEndTime = 804;

        TimeChangeDetails timing = new TimeChangeDetails();
        timing.setMaxEndTime(maxEndTime);

        assertTrue(maxEndTime == timing.getMaxEndTime().getValue());

        maxEndTime = 812;

        timing.setMaxEndTime(maxEndTime);

        assertTrue(maxEndTime == timing.getMaxEndTime().getValue());
    }

    @Test
    public void testSetLikelyTimePrimitive() {

        int likelyTime = 123;

        TimeChangeDetails timing = new TimeChangeDetails();
        timing.setLikelyTime(likelyTime);

        assertTrue(likelyTime == timing.getLikelyTime().getValue());

        likelyTime = 321;

        timing.setLikelyTime(likelyTime);

        assertTrue(likelyTime == timing.getLikelyTime().getValue());
    }

    @Test
    public void testSetConfidencePrimitive() {

        int confidence = 1;

        TimeChangeDetails timing = new TimeChangeDetails();
        timing.setConfidence(confidence);

        assertTrue(confidence == timing.getConfidence().getValue());

        confidence = 11;

        timing.setConfidence(confidence);

        assertTrue(confidence == timing.getConfidence().getValue());
    }

    @Test
    public void testSetNextTimePrimitive() {

        int nextTime = 999;

        TimeChangeDetails timing = new TimeChangeDetails();
        timing.setNextTime(nextTime);

        assertTrue(nextTime == timing.getNextTime().getValue());

        nextTime = 566;

        timing.setNextTime(nextTime);

        assertTrue(nextTime == timing.getNextTime().getValue());
    }

    @Test
    public void testEncodeUPERMin() {

        TimeMark minEndTime = new TimeMark(764);

        TimeChangeDetails timing = new TimeChangeDetails(minEndTime);

        String timingOptionals = "00000";
        String remainingBits = minEndTime.encodeUPER();
        assertTrue((timingOptionals + remainingBits).equals(timing.encodeUPER()));
    }

    @Test
    public void testEncodeUPERMax() {

        TimeMark startTime = new TimeMark(1);
        TimeMark minEndTime = new TimeMark(2);
        TimeMark maxEndTime = new TimeMark(3);
        TimeMark likelyTime = new TimeMark(4);
        TimeIntervalConfidence confidence = new TimeIntervalConfidence(5);
        TimeMark nextTime = new TimeMark(6);

        TimeChangeDetails timing = new TimeChangeDetails(minEndTime);
        timing.setStartTime(startTime);
        timing.setMaxEndTime(maxEndTime);
        timing.setLikelyTime(likelyTime);
        timing.setConfidence(confidence);
        timing.setNextTime(nextTime);

        String timingOptionals = "11111";
        String remainingBits = startTime.encodeUPER() + minEndTime.encodeUPER() + maxEndTime.encodeUPER() + likelyTime.encodeUPER() + confidence.encodeUPER() + nextTime.encodeUPER();
        assertTrue((timingOptionals + remainingBits).equals(timing.encodeUPER()));
    }

    @Test
    public void testDecodeUPERMin() {

        TimeMark minEndTime = new TimeMark(546);

        String timingOptionals = "00000";

        TimeChangeDetails timing = new TimeChangeDetails();
        String remainingBits = timing.decodeUPER(timingOptionals + minEndTime.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertNull(timing.getStartTime());
        assertNull(timing.getMaxEndTime());
        assertNull(timing.getLikelyTime());
        assertNull(timing.getConfidence());
        assertNull(timing.getNextTime());
        assertTrue(minEndTime.equals(timing.getMinEndTime()));
    }

    @Test
    public void testDecodeUPERMax() {

        TimeMark startTime = new TimeMark(6);
        TimeMark minEndTime = new TimeMark(5);
        TimeMark maxEndTime = new TimeMark(4);
        TimeMark likelyTime = new TimeMark(3);
        TimeIntervalConfidence confidence = new TimeIntervalConfidence(2);
        TimeMark nextTime = new TimeMark(1);

        String timingOptionals = "11111";

        TimeChangeDetails timing = new TimeChangeDetails();
        String remainingBits = timing
                .decodeUPER(timingOptionals + startTime.encodeUPER() + minEndTime.encodeUPER() + maxEndTime.encodeUPER() + likelyTime.encodeUPER() + confidence.encodeUPER() + nextTime.encodeUPER());
        assertTrue("".equals(remainingBits));

        assertTrue(startTime.equals(timing.getStartTime()));
        assertTrue(minEndTime.equals(timing.getMinEndTime()));
        assertTrue(maxEndTime.equals(timing.getMaxEndTime()));
        assertTrue(likelyTime.equals(timing.getLikelyTime()));
        assertTrue(confidence.equals(timing.getConfidence()));
        assertTrue(nextTime.equals(timing.getNextTime()));
    }

    @Test
    public void testDecodeUPERLessBits() {

        String timingOptionals = "0111";

        TimeChangeDetails timing = new TimeChangeDetails();
        thrown.expect(IllegalArgumentException.class);
        timing.decodeUPER(timingOptionals);
    }

    @Test
    public void testHashCode() {

        int startTime = timing.getStartTime().getValue();
        int minEndTime = timing.getMinEndTime().getValue();
        int maxEndTime = timing.getMaxEndTime().getValue();
        int likelyTime = timing.getLikelyTime().getValue();
        int confidence = timing.getConfidence().getValue();
        int nextTime = timing.getNextTime().getValue();

        TimeChangeDetails timing2 = new TimeChangeDetails(minEndTime + 1);
        timing2.setStartTime(startTime + 1);
        timing2.setMaxEndTime(maxEndTime + 1);
        timing2.setLikelyTime(likelyTime + 1);
        timing2.setConfidence(confidence + 1);
        timing2.setNextTime(nextTime + 1);

        assertFalse(timing.hashCode() == timing2.hashCode());
        assertTrue(timing.hashCode() == timing.hashCode());
        assertTrue(timing2.hashCode() == timing2.hashCode());

        TimeChangeDetails timing3 = new TimeChangeDetails(minEndTime);
        timing3.setStartTime(startTime);
        timing3.setMaxEndTime(maxEndTime);
        timing3.setLikelyTime(likelyTime);
        timing3.setConfidence(confidence);
        timing3.setNextTime(nextTime);

        assertTrue(timing.hashCode() == timing3.hashCode());
        assertFalse(timing2.hashCode() == timing3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(timing.equals(timing));
        assertFalse(timing.equals(null));
        assertFalse(timing.equals(new String()));

        int startTime = timing.getStartTime().getValue();
        int minEndTime = timing.getMinEndTime().getValue();
        int maxEndTime = timing.getMaxEndTime().getValue();
        int likelyTime = timing.getLikelyTime().getValue();
        int confidence = timing.getConfidence().getValue();
        int nextTime = timing.getNextTime().getValue();

        // different
        TimeChangeDetails timing2 = new TimeChangeDetails(minEndTime + 1);
        timing2.setStartTime(startTime + 1);
        timing2.setMaxEndTime(maxEndTime + 1);
        timing2.setLikelyTime(likelyTime + 1);
        timing2.setConfidence(confidence + 1);
        timing2.setNextTime(nextTime + 1);

        assertFalse(timing.equals(timing2));

        // different start time
        timing2 = new TimeChangeDetails(minEndTime);
        timing2.setStartTime(startTime + 1);
        timing2.setMaxEndTime(maxEndTime);
        timing2.setLikelyTime(likelyTime);
        timing2.setConfidence(confidence);
        timing2.setNextTime(nextTime);

        assertFalse(timing.equals(timing2));

        // different minimum end time
        timing2 = new TimeChangeDetails(minEndTime + 1);
        timing2.setStartTime(startTime);
        timing2.setMaxEndTime(maxEndTime);
        timing2.setLikelyTime(likelyTime);
        timing2.setConfidence(confidence);
        timing2.setNextTime(nextTime);

        assertFalse(timing.equals(timing2));

        // different maximum end time
        timing2 = new TimeChangeDetails(minEndTime);
        timing2.setStartTime(startTime);
        timing2.setMaxEndTime(maxEndTime + 1);
        timing2.setLikelyTime(likelyTime);
        timing2.setConfidence(confidence);
        timing2.setNextTime(nextTime);

        assertFalse(timing.equals(timing2));

        // different likely time
        timing2 = new TimeChangeDetails(minEndTime);
        timing2.setStartTime(startTime);
        timing2.setMaxEndTime(maxEndTime);
        timing2.setLikelyTime(likelyTime + 1);
        timing2.setConfidence(confidence);
        timing2.setNextTime(nextTime);

        assertFalse(timing.equals(timing2));

        // different confidence
        timing2 = new TimeChangeDetails(minEndTime);
        timing2.setStartTime(startTime);
        timing2.setMaxEndTime(maxEndTime);
        timing2.setLikelyTime(likelyTime);
        timing2.setConfidence(confidence + 1);
        timing2.setNextTime(nextTime);

        assertFalse(timing.equals(timing2));

        // different next time
        timing2 = new TimeChangeDetails(minEndTime);
        timing2.setStartTime(startTime);
        timing2.setMaxEndTime(maxEndTime);
        timing2.setLikelyTime(likelyTime);
        timing2.setConfidence(confidence);
        timing2.setNextTime(nextTime + 1);

        assertFalse(timing.equals(timing2));

        // same
        timing2 = new TimeChangeDetails(minEndTime);
        timing2.setStartTime(startTime);
        timing2.setMaxEndTime(maxEndTime);
        timing2.setLikelyTime(likelyTime);
        timing2.setConfidence(confidence);
        timing2.setNextTime(nextTime);

        assertTrue(timing.equals(timing2));
    }

    @Test
    public void testEqualsNull() {

        int startTime = timing.getStartTime().getValue();
        int minEndTime = timing.getMinEndTime().getValue();
        int maxEndTime = timing.getMaxEndTime().getValue();
        int likelyTime = timing.getLikelyTime().getValue();
        int confidence = timing.getConfidence().getValue();
        int nextTime = timing.getNextTime().getValue();

        TimeChangeDetails timing2 = new TimeChangeDetails(minEndTime);

        assertFalse(timing.equals(timing2));

        timing2.setStartTime(startTime);

        assertFalse(timing.equals(timing2));

        timing2.setMaxEndTime(maxEndTime);

        assertFalse(timing.equals(timing2));

        timing2.setLikelyTime(likelyTime);

        assertFalse(timing.equals(timing2));

        timing2.setConfidence(confidence);

        assertFalse(timing.equals(timing2));

        timing2.setNextTime(nextTime);

        assertTrue(timing.equals(timing2));
    }
}
