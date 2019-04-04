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

package org.etexascode.webapp.rest;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.List;
import java.util.jar.Attributes;
import java.util.jar.Manifest;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.interceptor.Interceptors;
import javax.json.Json;
import javax.json.JsonObject;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.etexascode.webapp.exception.WebAppException;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.io.FeedException;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.XmlReader;

/**
 * The REST service for general information requests.
 * 
 * @author bbadillo
 * @author keagan
 * @author emyers
 */
@Path("/info")
@RequestScoped
@Interceptors({ StringTrimmer.class })
public class InfoService {

    /** The servlet request. */
    @Inject
    private HttpServletRequest request;

    /** The servlet context. */
    @Context
    private ServletContext servletContext;

    /**
     * Returns eTEXAS blog content.
     * 
     * @param blogCount The number of blog entries to return.
     * @return A response (200) with the specified number of blog entries.
     * @throws IOException If the blog content cannot be read.
     */
    @GET
    @Path("/blogrss")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getBlogRss(@QueryParam("blogCount") @DefaultValue("5") int blogCount) throws IOException {

        JSONArray feedResults = new JSONArray();

        try {

            // get feed content
            URLConnection connection = new URL("http://etexas.harmonia.com/blog/?feed=rss2").openConnection();
            connection.setConnectTimeout(5000);
            SyndFeed feed = new SyndFeedInput().build(new XmlReader(connection));

            // build JSON objects for entries
            @SuppressWarnings("unchecked")
            List<SyndEntry> entries = feed.getEntries();
            Iterator<SyndEntry> iter = entries.iterator();
            for (int i = 0; i < blogCount && iter.hasNext(); i++) {

                SyndEntry entry = iter.next();
                JSONObject feedEntry = new JSONObject();
                feedEntry.put("title", entry.getTitle());
                feedEntry.put("description", entry.getDescription().getValue());
                feedEntry.put("date", new SimpleDateFormat("MMMM d, yyyy").format(entry.getPublishedDate()));
                feedEntry.put("link", entry.getUri());
                feedResults.put(feedEntry);
            }

            return Response.ok(feedResults.toString()).build();
        }
        catch (IllegalArgumentException | FeedException | JSONException exception) {

            LoggerFactory.getLogger(InfoService.class).error(exception.getLocalizedMessage(), exception);
            throw new WebApplicationException(Status.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Returns the application version information.
     * 
     * @return A response (200) with application version information.
     */
    @GET
    @Path("/version")
    @Produces(MediaType.APPLICATION_JSON)
    public Response getVersion() {

        try {

            // get version information from the Manifest file
            InputStream inputStream = this.servletContext.getResourceAsStream("/META-INF/MANIFEST.MF");
            Manifest manifest = new Manifest(inputStream);
            Attributes attributes = manifest.getMainAttributes();
            String title = attributes.getValue("Specification-Title");
            String version = attributes.getValue("Specification-Version");
            String build = attributes.getValue("Implementation-Version");
            inputStream.close();

            JsonObject response = Json.createObjectBuilder()
                    .add("title", title)
                    .add("version", version)
                    .add("build", build)
                    .build();

            return Response.ok(response.toString()).build();
        }
        catch (IOException exception) {

            LoggerFactory.getLogger(InfoService.class).error(exception.getLocalizedMessage(), exception);
            throw new WebApplicationException(Status.INTERNAL_SERVER_ERROR);
        }
    }

    /**
     * Returns the Web Start URI.
     * 
     * @return A response (200) with the Web Start URI.
     * @throws WebAppException If the Web Start URI cannot be retrieved.
     */
    @GET
    @Path("/webstart")
    @Produces({ MediaType.APPLICATION_JSON, MediaType.TEXT_PLAIN })
    public Response getWebStartURI() throws WebAppException {

        try {

            if (servletContext != null) {

                InitialContext context = new InitialContext();
                String scheme = request.getScheme();
                String host = servletContext.getInitParameter("VisibleHostName");
                int port = Integer.parseInt(servletContext.getInitParameter("VisibleHostPort"));
                String path = (String)context.lookup("etexas.webstartapp");

                URI uri = new URI(scheme, null, host, port, path, null, null);
                LoggerFactory.getLogger(InfoService.class).info(String.format("URI: %s", uri.toString()));
                return Response.ok(uri.toString()).build();
            }

            throw new IllegalStateException("null servlet context");
        }
        catch (URISyntaxException | NamingException exception) {

            LoggerFactory.getLogger(InfoService.class).debug(exception.toString());
            String message = (exception instanceof NamingException)
                    ? "The registered location could not be found."
                    : "The webstart URI could not be created.";

            throw new WebAppException("Web Start Failure", message);
        }
    }
}