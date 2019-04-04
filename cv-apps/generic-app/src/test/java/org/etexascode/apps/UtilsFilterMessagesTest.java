/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.apps;

import java.util.Collection;

import org.junit.Assert;

public class UtilsFilterMessagesTest {

    public class TestClassType {

        private final String testData = "data";

        public TestClassType() {}
    }

    public class TestClassType2 {

        private final String otherData = "jklfdskljfds";

        public TestClassType2() {}
    }

    public UtilsFilterMessagesTest() {
        super();
    }

    private Object[] genMessageList() {
        return new Object[] { new TestClassType(), new TestClassType2(), new TestClassType(), new TestClassType(), new TestClassType(), new TestClassType2() };
    }

    public void testFilterMessages() {
        Object[] messList = genMessageList();

        Collection<TestClassType> filtered1 = UtilsFilterMessages.filterMessages(TestClassType.class, messList);

        Assert.assertTrue(filtered1.size() == 4);

        Collection<TestClassType2> filtered2 = UtilsFilterMessages.filterMessages(TestClassType2.class, messList);

        Assert.assertTrue(filtered2.size() == 2);
    }
}
