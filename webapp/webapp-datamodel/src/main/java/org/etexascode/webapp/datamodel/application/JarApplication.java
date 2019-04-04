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
package org.etexascode.webapp.datamodel.application;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.etexascode.devicedata.CVAppFactory;
import org.etexascode.webapp.datamodel.FileData;

/**
 * A connected vehicle JAR application.
 * 
 * @author bbadillo
 * @author emyers
 */
@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@DiscriminatorValue(ApplicationType.Discriminator.JAR)
public class JarApplication extends Application {

    /** The serial version ID. */
    private static final long serialVersionUID = 1L;

    /** The name of this application in the source file. */
    @Column(name = "source_name")
    private String sourceName;

    /** The JAR file name for this application. */
    @Column(name = "file_name")
    private String fileName;

    /** The JAR file data for this application. */
    @XmlTransient
    @JoinColumn(name = "file")
    @ManyToOne(cascade = {}, fetch = FetchType.LAZY)
    private FileData fileData = new FileData();

    /**
     * Returns the name of this application in the source file.
     * 
     * @return The string name of this application in the source file.
     */
    public String getSourceName() {

        return sourceName;
    }

    /**
     * Sets the name of this application in the source file.
     * 
     * @param sourceName The string name in the source file to set.
     */
    public void setSourceName(String sourceName) {

        this.sourceName = sourceName;
    }

    /**
     * Returns the JAR file name for this application.
     * 
     * @return The string JAR file name for this application.
     */
    public String getFileName() {

        return fileName;
    }

    /**
     * Sets the JAR file name for this application.
     * 
     * @param fileName The string JAR file name to set.
     */
    public void setFileName(String fileName) {

        this.fileName = fileName;
    }

    /**
     * Returns the JAR file data for this application.
     * 
     * @return The JAR file data for this application.
     */
    public FileData getFileData() {

        return fileData;
    }

    /**
     * Sets the JAR file data for this application.
     * 
     * @param fileData The JAR file data to set.
     */
    public void setFileData(FileData fileData) {

        this.fileData = fileData;
    }

    @Override
    protected Application copyInternal() {

        JarApplication application = new JarApplication();
        application.setSourceName(this.getSourceName());
        application.setFileName(this.getFileName());
        application.setFileData(this.getFileData());
        return application;
    }

    @Override
    public Class<?> getInstanceClass() {

        if (fileData == null || fileData.getData() == null) {

            throw new RuntimeException("No file data is associated with this application.");
        }

        Map<String, Class<?>> classMap;

        try {

            classMap = CVAppFactory.loadJars(Arrays.asList(fileData.getData()), getClass().getClassLoader());
        }
        catch (IOException exception) {

            throw new RuntimeException("The file data could not be extracted for this application.");
        }

        return classMap.get(sourceName);
    }

    @Override
    public String[] getInstanceParameters() {

        List<ApplicationParameter> parameters = getParameters();
        String[] instanceParameters = new String[parameters.size()];

        for (int i = 0; i < parameters.size(); i++) {

            instanceParameters[i] = parameters.get(i).getValue();
        }

        return instanceParameters;
    }
}