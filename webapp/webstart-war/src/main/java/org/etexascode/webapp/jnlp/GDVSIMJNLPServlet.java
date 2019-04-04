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

package org.etexascode.webapp.jnlp;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.stream.StreamSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A servlet for dynamically generating a JNLP response to run GDVSIM using Java WebStart
 * 
 * @author bmauldon
 * @author bbadillo
 */
public class GDVSIMJNLPServlet extends HttpServlet {

    /** Static logger */
    private static final Logger LOGGER = LoggerFactory.getLogger(GDVSIMJNLPServlet.class);

    @Override
    public void init() throws ServletException {
        super.init();

        try {
            Context env = new InitialContext();
            env.rebind(Constants.JNDI_NAME_WEBSTARTAPP, getServletContext().getContextPath());
        }
        catch (NamingException ex) {
            LOGGER.debug(ex.toString());
        }
    }

    @Override
    public void service(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        String context;
        try {
            Context env = new InitialContext();
            context = (String)env.lookup(Constants.JNDI_NAME_WEBSERVICES);
        }
        catch (NamingException ex) {
            LOGGER.debug(ex.toString());
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Web services have not yet been registered");
            return;
        }

        String simId = request.getParameter("simId");
        if (simId == null || simId.isEmpty()) {
            String pathInfo = request.getPathInfo();
            String[] split = pathInfo.split("/");
            if (split.length != 3) {
                response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Missing request parameter: simId");
                return;
            }
            simId = split[1];
        }

        try {
            String hostname = getServletContext().getInitParameter("VisibleHostName");
            int port = Integer.parseInt(getServletContext().getInitParameter("VisibleHostPort"));
            URI uri = new URI(request.getScheme(), null, hostname, port, request.getContextPath(), null, null);
            LOGGER.info(String.format("Using WebStart URI: %s", uri.toString()));
            URI wsUri = new URI(request.getScheme(), null, hostname, port, context, null, null);
            LOGGER.info(String.format("Using Web Services URI: %s", uri.toString()));

            try {
                JAXBContext jaxbContext = JAXBContext.newInstance(JnlpType.class.getPackage().getName());

                Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();

                StreamSource source = new StreamSource(getServletContext().getResourceAsStream("/JNLP/gdvsimBase.jnlp"));
                JAXBElement<JnlpType> root = unmarshaller.unmarshal(source, JnlpType.class);
                JnlpType jnlpElement = root.getValue();

                jnlpElement.setCodebase(uri.toString() + "/JNLP");
                jnlpElement.setHref("gdvsim-servlet/" + simId + "/gdvsim.jnlp");

                List<String> argument = jnlpElement.getApplicationDesc().getArgument();
                argument.add(wsUri.toString() + "/rest/files/simulations/" + simId);

                StringWriter stringOut = new StringWriter();

                Marshaller marshaller = jaxbContext.createMarshaller();
                marshaller.marshal(root, stringOut);

                String attachment = "inline; filename=\"gdvsim-editor-" + System.currentTimeMillis() + ".jnlp\"";

                response.setContentType("application/x-java-jnlp-file");
                response.setHeader("Cache-Control", "max-age=30");
                response.setHeader("Content-disposition", attachment);

                OutputStreamWriter out = new OutputStreamWriter(response.getOutputStream());
                String outputXML = stringOut.toString();
                out.write(outputXML);
                out.flush();
                out.close();

            }
            catch (JAXBException ex) {
                LOGGER.debug(ex.toString());
                response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Error parsing JNLP file.");
            }
        }
        catch (URISyntaxException ex) {
            LOGGER.debug(ex.toString());
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Error creating a URI.");
        }
    }
}
