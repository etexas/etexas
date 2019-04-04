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
package org.etexascode.webapp.util;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.apache.commons.codec.binary.Base64;
import org.slf4j.LoggerFactory;

/**
 * Provides utility methods to encode string values.
 * 
 * @author emyers
 */
public final class StringEncoder {

    /* prevents instantiation */
    private StringEncoder() {}

    /**
     * Encodes a string using a 256 bit one way hash.
     * 
     * @param value The string value to encode.
     * @return The encoded string.
     */
    public static String hash(String value) {

        try {

            MessageDigest messageDigest = MessageDigest.getInstance("SHA-256");
            messageDigest.update(value.getBytes("UTF-8"));
            return Base64.encodeBase64String(messageDigest.digest());
        }
        catch (NoSuchAlgorithmException | UnsupportedEncodingException exception) {

            LoggerFactory.getLogger(StringEncoder.class).error(null, exception);
        }

        throw new AssertionError();
    }
}