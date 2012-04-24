package elw.dao;

import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.configuration.reloading.FileChangedReloadingStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

/**
 * Configuration is better to be exposed as
 * bean delegating back to CompositeConfiguration directly.
 * <p/>
 * Here we create/cache CompositeConfiguration instances
 * for any given baseName (which should be product/component-dependent).
 *
 * @see CompositeConfiguration
 */
public class ConfigLoader {
    private static final Logger log =
        LoggerFactory.getLogger(ConfigLoader.class);

    public static final int RELOAD_DELAY_MILLIS = 5 * 60 * 1000;
    public static final int RELOAD_DELAY_DEVMODE_MILLIS = 100;

    private static final Map<String, CompositeConfiguration> baseNameToCompositeConf =
            new TreeMap<String, CompositeConfiguration>();

    public static CompositeConfiguration getConfiguration(
        final String baseName
    ) {

        final boolean devMode = new File(
            System.getProperty("user.home"),
            "devmode"
        ).exists();

        synchronized (ConfigLoader.class) {

            final CompositeConfiguration existing =
                baseNameToCompositeConf.get(baseName);
            
            if (existing != null) {
                return existing;
            }

            final CompositeConfiguration created = loadConfig(
                baseName,
                devMode ?
                    RELOAD_DELAY_DEVMODE_MILLIS :
                    RELOAD_DELAY_MILLIS
            );

            baseNameToCompositeConf.put(baseName, created);

            return created;

        }
    }

    protected static CompositeConfiguration loadConfig(
        final String baseName,
        final int refreshDelay
    ) {
        try {

            return loadConfigChecked(baseName, refreshDelay);

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

    protected static CompositeConfiguration loadConfigChecked(
        final String baseName,
        final int refreshDelay
    ) throws MalformedURLException, ConfigurationException {

        final FileChangedReloadingStrategy reloadStrategy =
            new FileChangedReloadingStrategy();
        reloadStrategy.setRefreshDelay(refreshDelay);

        final Collection<Configuration> configurations =
            new ArrayList<Configuration>();

        final String fileName = baseName + ".properties";
        final File cwdFile =
            new File(System.getProperty("user.dir"), fileName);

        if (cwdFile.isFile()) {
            log.info("found config in {}", cwdFile.getPath());
            configurations.add(
                createConf(cwdFile.toURI().toURL(), reloadStrategy)
            );
        }

        final File homeFile =
            new File(System.getProperty("user.home"), fileName);
        if (homeFile.isFile()) {
            log.info("found config in {}", homeFile.getPath());
            configurations.add(
                createConf(homeFile.toURI().toURL(), reloadStrategy)
            );
        }

        final String osName = System.getProperty("os.name").toLowerCase();
        if (osName.contains("linux") || osName.contains("unix")) {
            final File etcDir = new File("/etc");
            if (etcDir.isDirectory()) {
                final File etcFile = new File(etcDir, fileName);
                if (etcFile.isFile()) {
                    log.info("found config in {}", etcFile.getPath());
                    configurations.add(
                        createConf(etcFile.toURI().toURL(), reloadStrategy)
                    );
                }
            }
        }

        final URL classpathUrl =
            ConfigLoader.class.getClassLoader().getResource("/" + fileName);
        if (classpathUrl != null) {
            log.info("found config in {}", classpathUrl);
            configurations.add(createConf(classpathUrl, null));
        }

        return new CompositeConfiguration(configurations);
    }

    protected static PropertiesConfiguration createConf(
        final URL resourceUrl,
        final FileChangedReloadingStrategy reloadStrategy
    ) throws ConfigurationException {

        final PropertiesConfiguration propConf =
            new PropertiesConfiguration();

        propConf.setListDelimiter((char) 0);
        propConf.setDelimiterParsingDisabled(true);
        if (reloadStrategy != null) {
            propConf.setReloadingStrategy(reloadStrategy);
        }
        propConf.load(resourceUrl);

        return propConf;
    }

}
