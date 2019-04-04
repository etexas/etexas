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
import java.net.URLEncoder;

import javax.mail.AuthenticationFailedException;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.NoSuchProviderException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Provides utility methods for user account operations.
 * 
 * @author ttevendale
 * @author emyers
 */
public class AccountProcessor {

    /**
     * Sends a password recovery email to the specified email address.
     * 
     * @param session The mail session.
     * @param baseUrl The base URL for the web application.
     * @param destinationAddress The string destination email address.
     * @param password The string password for the account.
     * @throws RuntimeException If the password recovery email cannot be sent.
     */
    public static void sendPasswordRecoveryEmail(Session session, String baseUrl, String destinationAddress, String password) {

        StringBuilder messageBuilder = new StringBuilder(512);
        messageBuilder.append(AccountProcessor.getSalutation());
        messageBuilder.append("As part of the password recovery process, the password for your account has been reset to the following value:<br><br>");
        messageBuilder.append(String.format("%s<br><br>", password));
        messageBuilder.append(String.format("Please <a href=\"%s\">sign in</a> and update the reset password as soon as possible. ", baseUrl));
        messageBuilder.append(AccountProcessor.getSignature());
        AccountProcessor.transportMessage(session, destinationAddress, "eTEXAS Password Recovery", messageBuilder.toString());
    }

    /**
     * Sends a username recovery email to the specified email address.
     * 
     * @param session The mail session.
     * @param destinationAddress The string destination email address.
     * @param username The string username for the account.
     * @throws RuntimeException If the username recovery email cannot be sent.
     */
    public static void sendUsernameRecoveryEmail(Session session, String destinationAddress, String username) {

        StringBuilder messageBuilder = new StringBuilder(384);
        messageBuilder.append(AccountProcessor.getSalutation());
        messageBuilder.append("As part of the username recovery process, the username for your account has been included below:<br><br>");
        messageBuilder.append(String.format("Username: %s<br><br>", username));
        messageBuilder.append(AccountProcessor.getSignature());
        AccountProcessor.transportMessage(session, destinationAddress, "eTEXAS Username Recovery", messageBuilder.toString());
    }

    /**
     * Sends a verification email to the specified email address.
     * 
     * @param session The mail session.
     * @param baseUrl The base URL for the web application.
     * @param destinationAddress The string destination email address.
     * @param token The registration token.
     * @throws RuntimeException If the verification email cannot be sent.
     */
    public static void sendVerificationEmail(Session session, String baseUrl, String destinationAddress, String token) {

        String destinationParameter;
        String tokenParameter;

        try {

            destinationParameter = URLEncoder.encode(destinationAddress, "UTF-8");
            tokenParameter = URLEncoder.encode(token, "UTF-8");
        }
        catch (UnsupportedEncodingException exception) {

            LoggerFactory.getLogger(AccountProcessor.class).debug("An Unsupported Encoding Exception occured during email verification", exception);
            throw new RuntimeException(exception);
        }

        String verifyUrl = String.format("%s/index.jsp?restCall=verifyEmail&email=%s&token=%s", baseUrl, destinationParameter, tokenParameter);

        StringBuilder messageBuilder = new StringBuilder(512);
        messageBuilder.append(AccountProcessor.getSalutation());
        messageBuilder.append("Thank you for registering a new eTEXAS account. ");
        messageBuilder.append(String.format("Please <a href=\"%s\">verify your email address</a> to activate your new account. ", verifyUrl));
        messageBuilder.append(AccountProcessor.getSignature());
        AccountProcessor.transportMessage(session, destinationAddress, "Welcome to eTEXAS", messageBuilder.toString());
    }

    /**
     * Returns the standard salutation for eTEXAS emails.
     * 
     * @return The string standard salutation for eTEXAS emails.
     */
    private static String getSalutation() {

        return "Greetings from eTEXAS,<br><br>";
    }

    /**
     * Returns the standard signature for eTEXAS emails.
     * 
     * @return The string standard signature for eTEXAS emails.
     */
    private static String getSignature() {

        return "If you believe you have received this email in error, please contact technical support.<br><br>eTEXAS<br><br>Note: emails sent to this address will not be answered. For technical support, please contact eTEXAS@harmonia.com.";
    }

    /**
     * Sends an email message to the specified email address.
     * 
     * @param session The mail session.
     * @param destinationAddress The string destination email address.
     * @param subject The string email subject.
     * @param text The string email text.
     * @throws RuntimeException If the email message cannot be sent.
     */
    private static void transportMessage(Session session, String destinationAddress, String subject, String text) {

        try {

            // compose message
            MimeMessage message = new MimeMessage(session);
            message.setFrom(new InternetAddress("etexastestmail@gmail.com"));
            message.addRecipient(Message.RecipientType.TO, new InternetAddress(destinationAddress));
            message.setSubject(subject);
            message.setText(text, "UTF-8", "html");
            Transport.send(message);
        }
        catch (MessagingException exception) {

            Logger logger = LoggerFactory.getLogger(AccountProcessor.class);

            if (exception instanceof AuthenticationFailedException) {

                logger.debug("The Authentication failed (probably connected to a bad email/password combination on the server side)", exception);
            }
            else if (exception instanceof NoSuchProviderException) {

                logger.debug("This fails when a session attempts to instantiate a Provider that doesn't exist", exception);
            }
            else {

                logger.debug("A Messaging Exception occured during email verification", exception);
            }

            throw new RuntimeException(exception);
        }
    }
}