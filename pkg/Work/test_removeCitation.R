use("tm.plugin.mail")
use("tm")
mails <- VCorpus(MBoxSource("~/Mail/bioC"))
mails[[7]]$content
removeCitation(mails[[7]])$content
