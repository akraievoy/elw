package elw.dao;

import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.PropertyPlaceholderConfigurer;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Properties;

public class PropertyPlaceholderConfCommons extends PropertyPlaceholderConfigurer {
    private static final Logger log =
            LoggerFactory.getLogger(PropertyPlaceholderConfCommons.class);

    @Override
    protected void processProperties(
            ConfigurableListableBeanFactory beanFactoryToProcess,
            Properties props
    ) throws BeansException {
        final CompositeConfiguration compConf = setupConfig();

        if (props == null) {
            props = new Properties();
        }

        for (Iterator<String> keyIt = compConf.getKeys(); keyIt.hasNext();) {
            final String key = keyIt.next();
            props.put(key, compConf.getString(key));
        }
        
        super.processProperties(beanFactoryToProcess, props);
    }

    protected CompositeConfiguration setupConfig() {
        try {
            return setupConfigChecked();
        } catch (MalformedURLException e) {
            throw new IllegalStateException(
                    "URLs formed this way should not be malformed, I think", e
            );
        } catch (ConfigurationException e) {
            throw new IllegalStateException(
                    "Not sure what is going on, thus propagate", e
            );
        }
    }

    protected CompositeConfiguration setupConfigChecked()
            throws MalformedURLException, ConfigurationException {
        final Collection<Configuration> configurations =
                new ArrayList<Configuration>();

        final File cwdFile =
                new File(System.getProperty("user.dir"), "elw.properties");

        if (cwdFile.isFile()) {
            log.info("found config in {}", cwdFile.getPath());
            configurations.add(new PropertiesConfiguration(cwdFile));
        }

        final File homeFile =
                new File(System.getProperty("user.home"), "elw.properties");
        if (homeFile.isFile()) {
            log.info("found config in {}", homeFile.getPath());
            configurations.add(new PropertiesConfiguration(homeFile));
        }

        final String osName = System.getProperty("os.name").toLowerCase();
        if (osName.contains("linux") || osName.contains("unix")) {
            final File etcDir = new File("/etc");
            if (etcDir.isDirectory()) {
                final File etcFile = new File(etcDir, "elw.properties");
                if (etcFile.isFile()) {
                    log.info("found config in {}", etcFile.getPath());
                    configurations.add(new PropertiesConfiguration(etcFile));
                }
            }
        }

        final URL classpathUrl =
                getClass().getClassLoader().getResource("/elw.properties");
        if (classpathUrl != null) {
            log.info("found config in {}", classpathUrl);
            configurations.add(new PropertiesConfiguration(classpathUrl));
        }

        return new CompositeConfiguration(configurations);
    }
}
