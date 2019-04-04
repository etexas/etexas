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
package org.etexascode.test.restharness;

import java.util.Map;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Form;

/**
 * This is a class of utility methods for connecting to eTEXAS REST services. General usage is
 * envisioned as running through the executeResource suite of methods.
 *
 * @author ablatt
 * @author bbadillo
 */
public class RESTUtils {

    /**
     * Opener for the http header.
     */
    public static final String http = "http://";

    /**
     * Static segment of the base url.
     */
    public static final String baseURL = ":8080/rest/";

    /**
     * Build a web resource out of only the rest function to be called and the service that contains
     * that function.
     *
     * @param hostLocation
     * @param service
     * @param function
     * @return The web resource.
     */
    public static WebTarget getResource(String hostLocation, String service, String function) {
        return getResource(http + hostLocation + baseURL + service + "/" + function);
    }

    /**
     * Build a web resource out of only the rest function to be called, the service that contains
     * that function and a parameter which should be on that path.
     *
     * @param hostLocation
     * @param service
     * @param function
     * @param pathParam
     * @return The web resource.
     */
    public static WebTarget getResource(String hostLocation, String service, String function, String pathParam) {
        return getResource(hostLocation, service, function + "/" + pathParam);
    }

    /**
     * Get a web resource from the path.
     *
     * @param path
     * @return The web resource.
     */
    public static WebTarget getResource(String path) {
        Client c = ClientBuilder.newClient();
        return c.target(path);
    }

    /**
     * Build a web resource out of only the rest function to be called and the service that contains
     * that function. Add the provided map of parameters to the resource. Return the resulting
     * resource.
     *
     * @param hostLocation
     * @param service
     * @param function
     * @param inParams
     * @return The web resource.
     */
    public static WebTarget getLoadedResource(String hostLocation, String service,
            String function, Map<String, String> inParams) {
        return addParams(getResource(hostLocation, service, function), inParams);
    }

    /**
     * Build a web resource out of only the rest function to be called, the service that contains
     * that function and a parameter which should be on that path. Add the provided map of
     * parameters to the resource. Return the resulting resource.
     *
     * @param hostLocation
     * @param service
     * @param function
     * @param pathParam
     * @param inParams
     * @return The web resource.
     */
    public static WebTarget getLoadedResource(String hostLocation, String service, String function,
            String pathParam, Map<String, String> inParams) {
        return addParams(getResource(hostLocation, service, function, pathParam), inParams);
    }

    /**
     * Add the map of parameters to the web resource. Return the result.
     *
     * @param resource
     * @param params
     * @return The web resource.
     */
    public static WebTarget addParams(WebTarget resource, Map<String, String> params) {
        for (String s : params.keySet()) {
            resource = resource.queryParam(s, params.get(s));
        }

        return resource;
    }

    /**
     * Execute the rest call encapsulated in the web resource to obtain results of type outClass.
     * Return the result.
     *
     * @param resource
     * @param outClass
     * @return The class.
     */
    public static <T> T executeResource(WebTarget resource, Class<T> outClass) {
        return resource.request().get(outClass);
    }

    /**
     * Execute the rest call encapsulated in the web resource to obtain results of type outClass.
     * Return the result.
     *
     * @param resource The resource.
     * @param outClass The return class type.
     * @return The class.
     */
    public static <T> T executeResourceDelete(WebTarget resource, Class<T> outClass) {
        return resource.request().delete(outClass);
    }

    /**
     * Execute the rest call encapsulated in the web resource to obtain results of type outClass.
     * Return the result.
     *
     * @param resource The WebResource.
     * @param outClass The return class type.
     * @param postParam The parameter that should be posted.
     * @return The class.
     */
    public static <T> T executeResource(WebTarget resource, Class<T> outClass, Object postParam) {
        return resource.request().post(Entity.json(postParam), outClass);
    }

    /**
     * Execute the rest call encapsulated in the web resource to obtain results of type outClass.
     * Return the result.
     *
     * @param resource The WebResource.
     * @param outClass The return class type.
     * @param postParam The parameter that should be posted.
     * @return The class.
     */
    public static <T> T executeResourceFormPost(WebTarget resource, Class<T> outClass, Form postParam) {
        return resource.request().post(Entity.form(postParam), outClass);
    }

    /**
     * Build a web resource out of only the rest function to be called and the service that contains
     * that function. Add the provided map of parameters to the resource. Execute the rest call
     * encapsulated in the web resource to obtain results of type outClass. Return the result.
     *
     * @param hostLocation
     * @param service
     * @param function
     * @param inParams
     * @param outClass
     * @return The class.
     */
    public static <T> T executeResource(String hostLocation, String service, String function,
            Map<String, String> inParams, Class<T> outClass) {
        return executeResource(getLoadedResource(hostLocation, service, function, inParams), outClass);
    }

    /**
     * Build a web resource out of only the rest function to be called and the service that contains
     * that function. Add the provided map of parameters to the resource. Execute the rest call
     * encapsulated in the web resource to obtain results of type outClass. Return the result.
     *
     * @param hostLocation The host location.
     * @param service The service name.
     * @param function The function from the service.
     * @param inParams The parameters.
     * @param outClass The return class type.
     * @param post The object to post.
     * @return The class.
     */
    public static <T> T executeResourceFormPost(String hostLocation, String service, String function,
            Map<String, String> inParams, Class<T> outClass, Form post) {
        return executeResourceFormPost(getLoadedResource(hostLocation, service, function, inParams), outClass, post);
    }

    /**
     * Build a web resource out of only the rest function to be called and the service that contains
     * that function. Add the provided map of parameters to the resource. Execute the rest call
     * encapsulated in the web resource to obtain results of type outClass. Return the result.
     *
     * @param hostLocation The host location.
     * @param service The service name.
     * @param function The function from the service.
     * @param inParams The parameters.
     * @param outClass The return class type.
     * @return The class.
     */
    public static <T> T executeResourceDelete(String hostLocation, String service, String function,
            Map<String, String> inParams, Class<T> outClass) {
        return executeResourceDelete(getLoadedResource(hostLocation, service, function, inParams), outClass);
    }

    /**
     * Build a web resource out of only the rest function to be called and the service that contains
     * that function. Add the provided map of parameters to the resource. Execute the rest call
     * encapsulated in the web resource to obtain results of type outClass. Return the result.
     *
     * @param hostLocation The host location.
     * @param service The service name.
     * @param function The function from the service.
     * @param inParams The parameters.
     * @param outClass The return class type.
     * @param post The object to post.
     * @return The class.
     */
    public static <T> T executeResource(String hostLocation, String service, String function,
            Map<String, String> inParams, Class<T> outClass, Object post) {
        return executeResource(getLoadedResource(hostLocation, service, function, inParams), outClass, post);
    }

    /**
     * Build a web resource out of only the rest function to be called, the service that contains
     * that function and a parameter which should be on that path. Add the provided map of
     * parameters to the resource. Execute the rest call encapsulated in the web resource to obtain
     * results of type outClass. Return the result.
     *
     * @param hostLocation
     * @param service
     * @param function
     * @param pathParam
     * @param inParams
     * @param outClass
     * @return The class.
     */
    public static <T> T executeResource(String hostLocation, String service, String function,
            String pathParam, Map<String, String> inParams, Class<T> outClass) {
        return executeResource(getLoadedResource(hostLocation, service, function, pathParam, inParams), outClass);
    }

    /**
     * Build a web resource out of only the rest function to be called and the service that contains
     * that function. Execute the rest call encapsulated in the web resource to obtain results of
     * type outClass. Return the result.
     *
     * @param hostLocation
     * @param service
     * @param function
     * @param outClass
     * @return The class.
     */
    public static <T> T executeResource(String hostLocation, String service, String function, Class<T> outClass) {
        return executeResource(getResource(hostLocation, service, function), outClass);
    }

    /**
     * Build a web resource out of only the rest function to be called, the service that contains
     * that function and a parameter which should be on that path. Execute the rest call
     * encapsulated in the web resource to obtain results of type outClass. Return the result.
     *
     * @param hostLocation
     * @param service
     * @param function
     * @param pathParam
     * @param outClass
     * @return The class.
     */
    public static <T> T executeResource(String hostLocation, String service, String function,
            String pathParam, Class<T> outClass) {
        return executeResource(getResource(hostLocation, service, function, pathParam), outClass);
    }
}
