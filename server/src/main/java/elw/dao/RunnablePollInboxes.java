package elw.dao;

import org.akraievoy.gear.G;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

public class RunnablePollInboxes implements Runnable {
	private static final Logger log = LoggerFactory.getLogger(RunnablePollInboxes.class);

	protected List<Dao> daos = new ArrayList<Dao>();
	protected long period = 60000;

	protected Thread worker;

	public void setDaos(List<Dao> daos) {
		this.daos = daos;
	}

	public void setPeriod(long period) {
		this.period = period;
	}

	public void run() {
		try {
			while (!Thread.currentThread().isInterrupted()) {
				Thread.sleep(period);
				for (Dao dao : daos) {
					try {
						dao.pollInbox();
					} catch (Exception e) {
						if (e instanceof InterruptedException) {
							throw (InterruptedException) e;
						}
						if (e.getCause() instanceof InterruptedException) {
							throw (InterruptedException) e.getCause();
						}
						log.warn("failed to poll inbox for {}:", dao.getMetaClass().getSimpleName(), G.report(e));
						log.info("failed", e);
					}
				}
			}
		} catch (InterruptedException e) {
			log.info("exiting");
		}
	}

	public void start() {
		worker = new Thread(this, this.getClass().getSimpleName());
		worker.start();
	}

	public void stop() {
		worker.interrupt();
	}
}
