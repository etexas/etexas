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
package org.etexascode.webapp.exception;

import java.io.IOException;

import javax.json.Json;
import javax.json.JsonObject;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;

import org.slf4j.LoggerFactory;

/**
 * Filters upload service calls to transform uncaught exceptions into the appropriate format.
 * 
 * @author bbadillo
 * @author emyers
 */
public class UploadExceptionFilter implements Filter {

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {}

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain next) throws IOException, ServletException {

        try {

            next.doFilter(servletRequest, servletResponse);
        }
        catch (UploadException exception) {

            LoggerFactory.getLogger(UploadExceptionFilter.class).info(exception.getTitle(), exception);
            ((HttpServletResponse)servletResponse).setStatus(exception.getStatus());
            servletResponse.setContentType(MediaType.APPLICATION_JSON);
            servletResponse.getWriter().print(exception.toJson().toString());
            servletResponse.getWriter().close();

        }
        catch (Exception exception) {

            String title = "eTEXAS Servlet Failure";
            String message = "An unknown servlet exception has occurred.";
            LoggerFactory.getLogger(UploadExceptionFilter.class).info(title, exception);

            JsonObject exceptionObject = Json.createObjectBuilder()
                    .add("exceptionType", exception.getClass().getSimpleName())
                    .add("statusCode", HttpServletResponse.SC_CONFLICT)
                    .add("title", title)
                    .add("messages", Json.createArrayBuilder().add(message).build())
                    .build();

            ((HttpServletResponse)servletResponse).setStatus(HttpServletResponse.SC_CONFLICT);
            servletResponse.setContentType(MediaType.APPLICATION_JSON);
            servletResponse.getWriter().print(exceptionObject.toString());
            servletResponse.getWriter().close();
        }
    }

    @Override
    public void destroy() {}
}