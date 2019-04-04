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
package org.etexascode.webapp.rest;

import javax.interceptor.AroundInvoke;
import javax.interceptor.Interceptor;
import javax.interceptor.InvocationContext;

/**
 * An interceptor to trim string parameter values for REST service methods.
 * 
 * @author emyers
 */
@Interceptor
public class StringTrimmer {

    /**
     * Replaces string parameter values with trimmed versions of the same value.
     * 
     * @param context The method invocation context.
     * @return The return value of the next method in the chain.
     * @throws Exception If the next method in the chain cannot be invoked.
     */
    @AroundInvoke
    public Object trimParameters(InvocationContext context) throws Exception {

        Object[] parameters = context.getParameters();
        Object[] trimmedParameters = new Object[parameters.length];

        for (int i = 0; i < parameters.length; i++) {

            Object parameter = parameters[i];
            trimmedParameters[i] = (parameter != null && parameter instanceof String) ? ((String)parameter).trim() : parameter;
        }

        context.setParameters(trimmedParameters);
        return context.proceed();
    }
}
