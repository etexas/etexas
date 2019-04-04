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
package org.etexascode.webapp.util;

import java.util.Random;

/**
 * Provides utility methods to generate random password strings.
 * 
 * @author emyers
 */
public class PasswordGenerator {

    /** The random generator for password characters. */
    private static final Random GENERATOR = new Random();

    // prevents instantiation
    private PasswordGenerator() {}

    /**
     * Generates a random password. Passwords are guaranteed to adhere to the password restrictions
     * for eTEXAS (at least 15 characters in length, at least one digit, at least one lowercase
     * letter, at least 1 uppercase letter, and at least 1 special character.
     * 
     * @return A string password that is randomly generated.
     */
    public static String generatePassword() {

        // password are between 15 and 30 characters in length
        int length = PasswordGenerator.GENERATOR.nextInt(16) + 15;
        StringBuilder passwordBuilder = new StringBuilder(length);

        for (int i = 0; i < length - 4; i++) {

            switch (PasswordGenerator.GENERATOR.nextInt(4)) {

                case 0:
                    passwordBuilder.append(PasswordGenerator.generateDigit());
                    break;

                case 1:
                    passwordBuilder.append(PasswordGenerator.generateLowercaseLetter());
                    break;

                case 2:
                    passwordBuilder.append(PasswordGenerator.generateUppercaseLetter());
                    break;

                case 3:
                    passwordBuilder.append(PasswordGenerator.generateSymbol());
                    break;

                default:
                    throw new IllegalStateException("Generate Password Failure");
            }
        }

        // ensure at least one of each character type was generated
        passwordBuilder.append(PasswordGenerator.generateLowercaseLetter());
        passwordBuilder.append(PasswordGenerator.generateUppercaseLetter());
        passwordBuilder.append(PasswordGenerator.generateSymbol());
        passwordBuilder.append(PasswordGenerator.generateDigit());

        return passwordBuilder.toString();
    }

    /**
     * Generates a random digit character.
     * 
     * @return A character that is a random digit.
     */
    private static char generateDigit() {

        return (char)(PasswordGenerator.GENERATOR.nextInt(10) + 48);
    }

    /**
     * Generates a random lowercase letter character.
     * 
     * @return A character that is a random lowercase letter.
     */
    private static char generateLowercaseLetter() {

        return (char)(PasswordGenerator.GENERATOR.nextInt(25) + 97);
    }

    /**
     * Generates a random uppercase letter character.
     * 
     * @return A character that is a random uppercase letter.
     */
    private static char generateUppercaseLetter() {

        return (char)(PasswordGenerator.GENERATOR.nextInt(25) + 65);
    }

    /**
     * Generates a random symbol (!#$%^&*) character.
     * 
     * @return A character that is a random symbol (!#$%^&*).
     */
    private static char generateSymbol() {

        switch (PasswordGenerator.GENERATOR.nextInt(7)) {

            case 0:
                return '!';

            case 1:
                return '#';

            case 2:
                return '$';

            case 3:
                return '%';

            case 4:
                return '&';

            case 5:
                return '*';

            case 6:
                return '^';

            default:
                throw new IllegalStateException("Generate Symbol Failure");
        }
    }
}
