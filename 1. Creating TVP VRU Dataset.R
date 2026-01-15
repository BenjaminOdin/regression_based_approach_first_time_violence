# Install Packages which will be used in the code:

install.packages("dplyr")
install.packages("caTools")
install.packages("pscl")
install.packages("ggplot2")
install.packages("metafor")
install.packages("survival")
install.packages("nortest")
install.packages("anytime")
install.packages("ROCR")
install.packages("flexsurv")
install.packages("timereg")
install.packages("lubridate")
install.packages("gt")
install.packages("tidyverse")
install.packages("openxlsx")
install.packages("ggrepel")
install.packages("knitr")
install.packages("kableExtra")
install.packages("purrr")
install.packages("survminer")
install.packages("boot")


# Import the relevant libraries having installed packages

library(dplyr)
library(stringr)
library(lubridate)
library(caTools)
library(ggplot2)
library(metafor)
library(survival)
library(nortest)
library(anytime)
library(ROCR)
library(flexsurv)
library(timereg)
library(lubridate)
library(gt)
library(tidyverse)
library(openxlsx)
library(ggrepel)
library(knitr)
library(kableExtra)
library(purrr)
library(survminer)
library(boot)


# Specify the path to the CSV file which contains the raw data from TVP:

file_path <- "C:/Users/T2303/Documents/TVP Research 2023 - 2024/10. Data/ben_odin_master_workbook.csv"


# Import the CSV file into RStudio:

data <- read.csv(file_path)


# Rename DF for ease of reference:

Personal_Occurence_DF_Descriptive <- data



# Recode Gender so that it is clear if males or females or unknown individuals are being considered:

Personal_Occurence_DF_Descriptive$Gender <- recode(Personal_Occurence_DF_Descriptive$Gender, 
                                                   "Male"="3", 
                                                   "Female"="2",
                                                   "Indeterminate" = "1", 
                                                   "null" = "1")


# For use in descriptive statistics looking at unknown individuals

Personal_Occurence_DF_Descriptive$Original.Gender <- recode(Personal_Occurence_DF_Descriptive$Original.Gender, 
                                                            "Male"="3", 
                                                            "Female"="2",
                                                            "Indeterminate" = "1", 
                                                            "null" = "1")


# Recode Ethnicity into groups - mainly this is done here to put numerical values to different ethnicities:

Personal_Occurence_DF_Descriptive$Ethnicity <- recode(Personal_Occurence_DF_Descriptive$Ethnicity, 
                                                      "1. White - North European" = "1",
                                                      "2. White - South European" = "1",
                                                      "3. Black" = "2",
                                                      "4. Asian" = "3",
                                                      "5. Chinese,Japanese,SE Asian" = "3",
                                                      "6. Middle Eastern" = "4")

# Recode Victim = 1 for Injured Party after agreement with TVP

Personal_Occurence_DF_Descriptive$Victim_status[Personal_Occurence_DF_Descriptive$Injured_party == 1] <- 1


# Ensure Time_of_incident is specified as a date

Personal_Occurence_DF_Descriptive$Time_of_incident <- as.Date(Personal_Occurence_DF_Descriptive$Time_of_incident, format = "%d/%m/%Y")


# Filter dataframe to only include the cohort aged 16 by 25th March 2021:

Personal_Occurence_DF_Descriptive <- subset(Personal_Occurence_DF_Descriptive, Personal_Occurence_DF_Descriptive$Age_cohort == 16)


# Recode Offences into higher-level categories:

Personal_Occurence_DF_Descriptive$Offence <- recode(Personal_Occurence_DF_Descriptive$Offence,
                                                    "Assault occasioning actual bodily harm"	= "ABH",
                                                    "Assault on Police: Assault occasioning actual bodily harm"	= "ABH",
                                                    "Theft of a motor vehicle" = "Acquisitive Offence",
                                                    "Theft in a dwelling other than from automatic machine or meter" = "Acquisitive Offence",
                                                    "Theft of pedal cycle"	= "Acquisitive Offence",
                                                    "Theft if not classified elsewhere"	= "Acquisitive Offence",
                                                    "Making off without payment"	= "Acquisitive Offence",
                                                    "Shoplifting"	= "Acquisitive Offence",
                                                    "Theft from motor vehicle"	= "Acquisitive Offence",
                                                    "Blackmail"	= "Acquisitive Offence",
                                                    "Theft from the person of another - Theft Only"	= "Acquisitive Offence",
                                                    "Acquisition use and possession"	= "Acquisitive Offence",
                                                    "Aggravated vehicle taking"	= "Acquisitive Offence",
                                                    "Unauthorised taking of a motor vehicle"	= "Acquisitive Offence",
                                                    "Theft by an employee" = "Acquisitive Offence",
                                                    "Theft of mail bag or postal packet  unlawfully taking away or opening of mail bag"	= "Acquisitive Offence",
                                                    "Prohibition on the importation / exportation of false identity documents"	= "Acquisitive Offence",
                                                    "Theft Or Unauthorised Taking Of Motor Vehicle"	= "Acquisitive Offence",
                                                    "Other Theft"	= "Acquisitive Offence",
                                                    "Possess/control articles for use in fraud"	= "Acquisitive Offence",
                                                    "Theft From The Person - Theft Only"	= "Acquisitive Offence",
                                                    "Pass counterfeit coin or note as genuine"	= "Acquisitive Offence",
                                                    "Theft from the person of another - Snatch" = "Acquisitive Offence",
                                                    "Receiving stolen goods" =	"Acquisitive Offence",
                                                    "Undertaking or assisting in the retention removal disposal of stolen goods or arranging to do so"	= "Acquisitive Offence",
                                                    "Theft From Vehicle"	= "Acquisitive Offence",
                                                    "Making or suppling articles for use in fraud"	= "Acquisitive Offence",
                                                    "Removal of articles from places open to public"	= "Acquisitive Offence",
                                                    "Theft"	= "Acquisitive Offence",
                                                    "Theft from automatic machine or meter"	= "Acquisitive Offence",
                                                    "Possess/control a false/improperly obtained/ another persons identity document"	= "Acquisitive Offence",
                                                    "Abstracting electricity"	= "Acquisitive Offence",
                                                    "Theft of conveyance other than motor vehicle or pedal cycle"	= "Acquisitive Offence",
                                                    "Forgery / Fraud (Not Action Fraud)"	= "Acquisitive Offence",
                                                    "Aggravated vehicle taking where the only aggravating factor is criminal damage of 5000 or under"	= "Acquisitive Offence",
                                                    "Theft Or Unauthorised Taking Of Pedal Cycle"	= "Acquisitive Offence",
                                                    "Theft In A Dwelling"	= "Acquisitive Offence",
                                                    "Concealing etc criminal property"	= "Acquisitive Offence",
                                                    "Take or ride a pedal cycle without consent"	= "Acquisitive Offence",
                                                    "Possess counterfeit coin or note"	= "Acquisitive Offence",
                                                    "Aggravated vehicle taking (driving/being carried) - only aggravating factor is criminal damage of property Â£5000 or under"	= "Acquisitive Offence",
                                                    "Arranges knows suspects retains the use or control of criminal property by or for another person"	= "Acquisitive Offence",
                                                    "Unauthorised taking of conveyance other than motor vehicle or pedal cycle"	= "Acquisitive Offence",
                                                    "Fraud/Forgery associated with registration and licensing documents"	= "Acquisitive Offence",
                                                    "Fraud - Vehicle/Driver Document"	= "Acquisitive Offence",
                                                    "Fraud/Forgery associated with insurance certificate"	= "Acquisitive Offence",
                                                    "Possess/control identity documents with intent"	= "Acquisitive Offence",
                                                    "Handling/Receiving Stolen Goods"	= "Acquisitive Offence",
                                                    "Theft Or Unauthorised Taking Of Mail"	= "Acquisitive Offence",
                                                    "Fraud/Forgery associated with driving licence"	= "Acquisitive Offence",
                                                    "Obtain benefits for himself or anyone else by making dishonest representations"	= "Acquisitive Offence",
                                                    "Cheating at gambling or enabling or assisting a person to cheat"	= "Acquisitive Offence",
                                                    "Theft from vehicle other than motor vehicle"	= "Acquisitive Offence",
                                                    "Theft From The Person - Snatch"	= "Acquisitive Offence",
                                                    "Reproduce British currency note or make imitation coins"	= "Acquisitive Offence",
                                                    "Failure to disclose another person involved in money laundering  regulated sector"	= "Acquisitive Offence",
                                                    "Forgery Other Than Of Drug Prescription"	= "Acquisitive Offence",
                                                    "Possession Of Items For Use In Fraud"	= "Acquisitive Offence",
                                                    "Intentionally deceive by forgery or document issued Sec 7"	= "Acquisitive Offence",
                                                    "Threats to kill" =	"Assault and Battery",
                                                    "Threats To Life"	=	"Assault and Battery",
                                                    "Assault on police: Common assault and battery"	=	"Assault and Battery",
                                                    "Assault Without Injury" = "Assault and Battery",
                                                    "Assault with Injury" = "ABH",
                                                    "Assault on constable police act 1996" = "Assault and Battery",
                                                    "Assault on other emergency service worker: Common assault and battery" = "Assault and Battery",
                                                    "Intimidating or intending to intimidate a witness" = "Assault and Battery",
                                                    "Assault Police" = "Assault and Battery",
                                                    "Intimidating juror witness or person assisting in investigation of offence" = "Assault and Battery",
                                                    "Harming/threatening to harm a witness" = "Assault and Battery",
                                                    "Assault with intent to resist apprehension" = "Assault and Battery",
                                                    "Assault a designated person or his assistant in the exercise of a relevant power" = "Assault and Battery",
                                                    "Assault on county court officer" = "Assault and Battery",
                                                    "Harming or threat to harm juror witness or person assisting in investigation of offence" = "Assault and Battery",
                                                    "Assault Without Injury On Constable" = "Assault and Battery",
                                                    "Assaulting a designated or accredited person in the execution of their duty" = "Assault and Battery",
                                                    "Assault on prison custody officer or officer in secure training centre" = "Assault and Battery",
                                                    "Causing danger to road users" = "Other - Driving",
                                                    "Assault on Police: Cause GBH with intent to resist arrest" = "GBH",
                                                    "Common assault and battery" = "Assault and Battery",
                                                    "Racially and/or religiously aggravated harassment fear of violence" = "Assault and Battery - Hate",
                                                    "Racially and/or religiously aggravated common assault or beating" = "Assault and Battery - Hate",
                                                    "Racially and/or religiously aggravated ABH" = "ABH",
                                                    "Racially and/or Religiously aggravated wounding/gbh" = "GBH",
                                                    "Racially and/or religiously aggravated intentional harassment alarm or distress" = "Assault and Battery (Public Order - Hate)",
                                                    "Racially and/or religiously aggravated fear/provocation of violence 9B" = "Assault and Battery (Public Order - Hate)",
                                                    "Affray" = "Assault and Battery (Public Order)",
                                                    "Violent disorder" = "Assault and Battery (Public Order)",
                                                    "Public Order: Fear or provocation of violence" = "Assault and Battery (Public Order)",
                                                    "Public Order Act Offences (Secs 4, 4A And 5) Alarm Or Distress/Fear Or Provocation Of Violence" = "Assault and Battery (Public Order)",
                                                    "Riot" = "Public Order",
                                                    "Dangerous Dog" =	"Other - Unclear",
                                                    "Threaten with a blade or sharply pointed article on school premisies" =	"Assault and Battery with a weapon",
                                                    "Owner or person in charge allowing dog  to be dangerously out of control in Any place in England and Wales (whether or not in a public place) injuring any person or assistance dog" =	"Assault and Battery with a weapon",
                                                    "Administering poison with intent to injure or annoy" =	"Assault and Battery with a weapon",
                                                    "Possess firearm or imitation firearm with intent to cause fear of violence Group I" =	"Assault and Battery with a weapon",
                                                    "Possession of firearm with intent to injure Group I" =	"Firearm - Possession",
                                                    "Threaten with a blade or sharply pointed article in a public place" =	"Assault and Battery with a weapon",
                                                    "Threaten with an offensive weapon on school premises" =	"Assault and Battery with a weapon",
                                                    "Possess firearm or imitation firearm with intent to cause fear of violence Group III" =	"Assault and Battery with a weapon",
                                                    "Threaten with an offensive weapon in a public place" =	"Assault and Battery with a weapon",
                                                    "Person in charge allowing dog to enter a nonpublic place and injure person" =	"Assault and Battery with a weapon",
                                                    "Possession of firearm with intent to injure Group III" =	"Firearm - Possession",
                                                    "Administering a substance with intent to commit a sexual offence" = "Sexual Offence",
                                                    "Possess firearm or imitation firearm with intent to cause fear of violence Group II" = "Assault and Battery with a weapon",
                                                    "Use of noxious substances or things to cause harm and intimidate" = "Assault and Battery with a weapon",
                                                    "Breach of restraining order" = "Breach of Order - Harrassment", 
                                                    "Breach of nonmolestation order" = "Breach of Order - Harrassment",  
                                                    "Breach of Restraining Order issued on acquittal  PfH Act" = "Breach of Order - Harrassment",  
                                                    "Breach conditions of injunction against harassment" = "Breach of Order - Harrassment", 
                                                    "Bail Offences" = "Breach of Order - Other",
                                                    "Failure to comply with Notification Order" = "Breach of Order - Other",
                                                    "Offence of breach of pre-charge bail conditions relating to travel" = "Breach of Order - Other",
                                                    "ASB - Breach of a Civil Injunction" = "Breach of Order - Other",
                                                    "Breach of a Criminal Behaviour Order" = "Breach of Order - Other",
                                                    "ASB - Breach of a Closure Order" = "Breach of Order - Other",
                                                    "Sex Offender Offences" = "Sexual Offence",
                                                    "ASB - Breach of a Section 35 Dispersal Order" = "Breach of Order - Other",
                                                    "Breach of antisocial behaviour order" = "Breach of Order - Other",
                                                    "ASB - Breach of a Public Spaces Protection Order" = "Breach of Order - Other",
                                                    "ASB - Breach of a Community Protection Notice" = "Breach of Order - Other",
                                                    "ASB - Breach of a Criminal Behaviour Order" = "Breach of Order - Other",
                                                    "ASB - Breach of a Closure Notice" = "Breach of Order - Other",
                                                    "Offender Management Act offences" = "Breach of Order - Other",
                                                    "Burglary In A Dwelling (Excluding Attempts)" = "Burglary",
                                                    "Burglary Residential (excluding Attempts) - Sheds/Garages etc" = "Burglary",
                                                    "Burglary Residential (excluding Attempts) - Dwellings" = "Burglary",
                                                    "Domestic Burglary - Theft or criminal damage only" = "Burglary",
                                                    "Burglary Residential (Attempts Only) - Dwellings" = "Burglary",
                                                    "Domestic Burglary (Attempts Only) - Theft or criminal damage only" = "Burglary",
                                                    "Aggravated burglary in a dwelling" = "Burglary",
                                                    "Burglary - Business and Community (Excluding Attempts)" = "Burglary",
                                                    "Burglary Residential (Distraction) - Dwellings" = "Burglary",
                                                    "Domestic Burglary (Distraction) - Theft or criminal damage only" = "Burglary",
                                                    "Aggravated Burglary - Residential - Dwellings" = "Burglary",
                                                    "Burglary non dwelling (Attempts Only) - with intent to steal" = "Burglary",
                                                    "Burglary non dwelling - Theft only" = "Burglary",
                                                    "Burglary - Business and Community (Attempts Only)" = "Burglary",
                                                    "Aggravated Burglary - Business and Community" = "Burglary",
                                                    "Burglary non dwelling - with intent to commit or with the commission of an offence" = "Burglary",
                                                    "Burglary Residential" = "Burglary",
                                                    "Domestic Burglary with the commission of an indictable only offence or with violence or the threat of violence" = "Burglary",
                                                    "Burglary Other Than In A Dwelling (Attempts Only)" = "Burglary",
                                                    "Domestic Burglary (Attempts Only) with the commission of an indictable only offence or with violence or the threat of violence" = "Burglary",
                                                    "Domestic Burglary (Attempted Distraction only) with intent to commit an indictable only offence" = "Burglary",
                                                    "Burglary Residential (Attempts Only) - Sheds/Garages etc" = "Burglary",
                                                    "Burglary Other Than In A Dwelling (Excluding Attempts)" = "Burglary",
                                                    "Burglary Residential (Attempted Distraction only) - Dwellings" = "Burglary",
                                                    "Burglary In A Dwelling - Distraction (Excluding Attempts)" = "Burglary",
                                                    "Burglary Residential (Distraction) - Sheds/Garages etc" = "Burglary",
                                                    "Aggravated Burglary - Residential - Sheds/Garages etc" = "Burglary",
                                                    "Burglary non dwelling (Attempts Only) - with intent to commit or with the commission of an offence" = "Burglary",
                                                    "Domestic Burglary with intent to commit an indictable only offence" = "Burglary",
                                                    "Burglary Residential â€“ Distraction" = "Burglary",
                                                    "Burglary In A Dwelling (Attempts Only)" = "Burglary",
                                                    "Burglary Business / Community" = "Burglary",
                                                    "Domestic Burglary (Distraction) with intent to commit an indictable only offence" = "Burglary",
                                                    "Domestic Burglary (Attempted Distraction only) with the commission of an indictable only offence or with violence or the threat of violence" = "Burglary",
                                                    "Aggravated burglary other than in a dwelling" = "Burglary",
                                                    "Domestic Burglary (Attempts Only) with intent to commit an indictable only offence" = "Burglary",
                                                    "Other criminal damage under Â£5000 other" =	"Criminal Damage",
                                                    "Other criminal damage under Â£5000 dwelling" =	"Criminal Damage",
                                                    "Criminal Damage" =	"Criminal Damage",
                                                    "Other criminal damage under Â£5000 vehicle" =	"Criminal Damage",
                                                    "Threats to destroy or damage property" =	"Criminal Damage",
                                                    "Arson not endangering life" =	"Criminal Damage",
                                                    "Arson endangering life" =	"Criminal Damage",
                                                    "Other criminal damage under Â£5000 other building" =	"Criminal Damage",
                                                    "Other criminal damage over Â£5000 dwelling" =	"Criminal Damage",
                                                    "Criminal Damage - Other" =	"Criminal Damage",
                                                    "Arson" =	"Criminal Damage",
                                                    "Other criminal damage over Â£5000 vehicle" =	"Criminal Damage",
                                                    "Criminal Damage To Dwellings" =	"Criminal Damage",
                                                    "Possess anything with intent to destroy or damage property" =	"Criminal Damage",
                                                    "Other criminal damage over Â£5000 other building" =	"Criminal Damage",
                                                    "Racial/Religious crim/damage to dwelling/building/vehicle/other" =	"Criminal Damage",
                                                    "Other criminal damage over Â£5000 other" =	"Criminal Damage",
                                                    "Criminal damage endangering life vehicle" =	"Criminal Damage",
                                                    "Criminal Damage To Vehicles" =	"Criminal Damage",
                                                    "Criminal Damage To Other Buildings" =	"Criminal Damage",
                                                    "Criminal damage endangering life other" =	"Criminal Damage",
                                                    "Criminal damage endangering life dwelling" =	"Criminal Damage",
                                                    "Criminal damage endangering life other buildings" =	"Criminal Damage",
                                                    "Drugs wef 26/1/09 Possession of cannabis class B" = "Drugs related",
                                                    "Having possession of a controlled drug  heroin" = "Drugs related",
                                                    "Drug Possession - Cannabis" = "Drugs related",
                                                    "Having possession of a controlled drug  other class C" = "Drugs related",
                                                    "Having possession of a controlled drug  cocaine" = "Drugs related",
                                                    "Having possession of a controlled drug  MDMA" = "Drugs related",
                                                    "Having possession of a controlled drug  unspecified class" = "Drugs related",
                                                    "Drug Offences" = "Drugs related",
                                                    "Having possession of a controlled drug  other class A" = "Drugs related",
                                                    "Drug Possession - Excluding Cannabis" = "Drugs related",
                                                    "Possession of a controlled drug  Synthetic Cannabinoid" = "Drugs related",
                                                    "Having possession of a controlled drug  LSD" = "Drugs related",
                                                    "Having possession of a controlled drug  other class B" = "Drugs related",
                                                    "Having possession of a controlled drug  Ketamine" = "Drugs related",
                                                    "Drug Offences Not Separately Classified" = "Drugs related",
                                                    "Having possession of a controlled drug  methadone" = "Drugs related",
                                                    "Having possession of a controlled drug  crack" = "Drugs related",
                                                    "Class B Possession of Cathinone derivative incl Mephedrone" = "Drugs related",
                                                    "Having possession of a controlled drug  amphetamine" = "Drugs related",
                                                    "Having possession of a controlled drug  Crystal Meths" = "Drugs related",
                                                    "Possession of a controlled drug  Piperazines incl BZP" = "Drugs related",
                                                    "Forgery of drug prescription or copying a false drug prescription" = "Drugs related",
                                                    "Supply or offering to supply a controlled drug  Amphetamine" = "Drugs related",
                                                    "False imprisonment" = "Exploitation and Trafficking",
                                                    "Kidnapping" = "Exploitation and Trafficking",
                                                    "Require person to perform forced or compulsory labour" = "Exploitation and Trafficking",
                                                    "Modern Slavery" = "Exploitation and Trafficking",
                                                    "Hold person in slavery or servitude" = "Exploitation and Trafficking",
                                                    "Arrange or facilitate travel of another person with a view to exploitation" = "Exploitation and Trafficking",
                                                    "N200/2 â€“ Report of Modern Slavery: NRM referral negative reasonable grounds decisionÂ" = "Exploitation and Trafficking",
                                                    "N200/1 â€“ Report of Modern Slavery: NRM referral pending reasonable grounds decisionÂ" = "Exploitation and Trafficking",
                                                    "N200/5 â€“ Report of Modern Slavery: NRM referral â€“ Negative reasonable grounds â€“ Outside England and Wales" = "Exploitation and Trafficking",
                                                    "N200/4 â€“ Report of Modern Slavery: NRM referral - positive reasonable grounds/Police Referral - Outside England and WalesÂ Â" = "Exploitation and Trafficking",
                                                    "N200/3 â€“ Report of Modern Slavery: NRM referral - duty to notify onlyÂ" = "Exploitation and Trafficking",
                                                    "N200/6 â€“ Report of Modern Slavery: NRM referral â€“ transferred to another force in England and Wales." = "Exploitation and Trafficking",
                                                    "Trafficking for Sexual Exploitation" = "Exploitation and Trafficking",
                                                    "Commit offence other than kidnapping or false imprisonment with intention of arranging travel with view to exploitation" = "Exploitation and Trafficking",
                                                    "Action Fraud - Call For Service" = "Financial Crime",
                                                    "Action Fraud - Referral" = "Financial Crime",
                                                    "TEW Offences Money Laundering, Terrorist Financing and Transfer of Funds (Information on the Payer) Regulations 2017" = "Financial Crime",
                                                    "All other TEW offences except under class 95/08  Financial Services  Markets Act" = "Financial Crime",
                                                    "Making counterfeit coin or note" = "Financial Crime",
                                                    "Possess firearm or imitation firearm with intent to committing indictable offence or resist arrest Group I" = "Firearm - Possession",
                                                    "Carry loaded Firearm or any other firearm and its ammunition in public place Group II" = "Firearm - Possession",
                                                    "Carry a loaded/unloaded or imitation firearm or airweapon in a public place" = "Firearm - Possession",
                                                    "Firearms Acts Offences" = "Firearm - Possession",
                                                    "Import prohibited weapons/ammunition with intent to evade prohibition/restriction" = "Firearm - Possession",
                                                    "Possess Firearm/ammunition without certificate Group I" = "Firearm - Possession",
                                                    "Possession of Firearms Offences" = "Firearm - Possession",
                                                    "Possession of Firearms With Intent" = "Firearm - Possession",
                                                    "Possessing or distributing firearm disguised as other object" = "Firearm - Possession",
                                                    "Possess firearm or imitation firearm while committing or being arrested for offence Group I" = "Firearm - Possession",
                                                    "Carry loaded Firearm or any other firearm and its ammunition in public place Group I" = "Firearm - Possession",
                                                    "Possess firearm or imitation firearm while committing or being arrested for offence Group III" = "Firearm - Possession",
                                                    "Possession of Firearm by persons previously convicted of a crime Group III" = "Firearm - Possession",
                                                    "Possess firearm or imitation firearm with intent to committing indictable offence or resist arrest Group III" = "Firearm - Possession",
                                                    "Possess shotgun without certificate Group II" = "Firearm - Possession",
                                                    "Possess/distribute prohibited weapon or ammunition Group I" = "Firearm - Possession",
                                                    "Possess firearm or imitation firearm while committing or being arrested for offence Group II" = "Firearm - Possession",
                                                    "Grievous Bodily Harm With Intent (Sec 18)" = "GBH",
                                                    "Wounding with intent to do GBH" = "GBH",
                                                    "Wound or inflict GBH with or without weapon" = "GBH",
                                                    "Causing death by dangerous driving" = "Manslaughter",
                                                    "Assault on Police: Wounding with intent to do GBH" = "GBH",
                                                    "Attempted murder; Attempted genocide or crime against humanity" = "Attempted Murder",
                                                    "Murder of persons aged 1 year or over; Genocide or Crime against humanity" = "Murder",
                                                    "Assist offender by impeding his apprehension or prosecution in a case of murder" = "Other - Unclear",
                                                    "Assault on Police: Wound or inflict GBH with or without weapon" = "GBH",
                                                    "Causing serious injury by dangerous driving" = "GBH",
                                                    "Manslaughter" = "Manslaughter",
                                                    "Death By Dangerous Driving" = "Manslaughter",
                                                    "Assault on Police: Wounding with intent to resist arrest" = "GBH",
                                                    "Child destruction" = "Other - Unclear",
                                                    "Soliciting / conspiracy to commit murder" = "Murder",
                                                    "Murder - Attempted" = "Murder",
                                                    "Murder of persons under 1 year of age" = "Murder",
                                                    "Assault Wounding Other - Endangering Life" = "GBH",
                                                    "Administering poison so as to endanger life" = "GBH",
                                                    "Harassment protection from harassment etc" = "Harrassment",
                                                    "Engage in controlling/coercive behaviour in an intimate / family relationship" = "Assault and Battery",
                                                    "Nuisance Messages" = "Harrassment",
                                                    "Pursue course of conduct in breach of S1 1 which amounts to stalking" = "Harrassment", 
                                                    "Harassment etc of a person in his home" = "Harrassment", 
                                                    "Harassment (First Single Incident) Non Recordable Crime" = "Harrassment",
                                                    "Harassment: Putting people in fear of violence" = "Assault and Battery",
                                                    "Malicious Communications" = "Harrassment", 
                                                    "Stalking involving serious alarm/distress" = "Harrassment",
                                                    "Sending letters etc with intent to cause distress or anxiety" = "Harrassment",
                                                    "Public Order: Cause intentional harassment alarm distress" = "Harrassment",
                                                    "Stalking / Harassment" = "Harrassment", 
                                                    "Stalking involving fear of violence" = "Assault and Battery",
                                                    "Racially and/or religiously aggravated harassment" = "Harrassment",
                                                    "Harassment - All Offences Under Harassment Act" = "Harrassment",
                                                    "Stalking Offences" = "Harrassment",
                                                    "Racially Aggravated Harassment (Sec 32 Crime And Disorder Act)" = "Harrassment", 
                                                    "Offences relating to notification - Stalking Protection Act 2019" = "Harrassment", 
                                                    "Sex with adult relative 18 offender 16 Penetration" = "Incest",
                                                    "Having an article with a blade or point in a public place" = "Knife - Possession",
                                                    "Possession of other offensive weapon on school premises" = "Knife - Possession",
                                                    "Having an article with a blade or point on school premises" = "Knife - Possession",
                                                    "Unauthorised possession in prison of knife or offensive weapon" = "Knife - Possession",
                                                    "null"	= "NA",
                                                    "Possession of Article with Blade or Point" = "Knife - Possession",
                                                    "Child Protection" = "Neglect",
                                                    "Adult Protection" = "Neglect",
                                                    "Cruelty to and neglect of children" = "Neglect",
                                                    "Abduction of a child by other persons" = "Exploitation and Trafficking",
                                                    "Abduction of a child by parent" = "Neglect",
                                                    "Excise infibulate aid abet counsel mutilation of female genitalia" = "GBH",
                                                    "Care worker ill-treat / wilfully neglect an individual" = "Neglect",
                                                    "Ill treatment or neglect of a person lacking capacity" = "Neglect",
                                                    "Care provider breach of duty of care resulting in ill-treatment/neglect of individual" = "Neglect",
                                                    "Cause/allow a child or vulnerable adult to suffer serious physical harm" = "Neglect",
                                                    "Action Fraud - Victim Care" = "Not a crime",
                                                    "ASB non-crime - High risk" = "Not a crime",
                                                    "Assist offender  indictment only" = "Other - Aiding and Abetting",
                                                    "Assist offender  either way offences" = "Other - Aiding and Abetting",
                                                    "Assisting a detained person to escape" = "Other - Aiding and Abetting",
                                                    "Recklessly / negligently act in manner likely to endanger aircraft / person in an aircraft." = "Other - Aircraft",
                                                    "Endangering safety of aircraft" = "Other - Aircraft",
                                                    "Intentionally interfere with performance of aircraft crew memberâ€™s duty." = "Other - Aircraft",
                                                    "Young person persistently possessing alcohol in a public place - CRI" = "Other - Alcohol",
                                                    "Sell alcohol to person under 18 PND - CRI" = "Other - Alcohol",
                                                    "Delivery of alcohol (or allowing delivery) to person under 18 PND - CRI" = "Other - Alcohol",
                                                    "Purchase in licensed premises of alcohol for person under 18 PND - CRI" = "Other - Alcohol",
                                                    "Consumption alcohol by person under 18 in licensed premises PND - CRI" = "Other - Alcohol",
                                                    "ASB non-crime - Medium risk" = "Other - ASB",
                                                    "ASB - Community" = "Other - ASB",
                                                    "ASB - Personal" = "Other - ASB",
                                                    "ASB non-crime related" = "Other - ASB",
                                                    "ASB - Environmental" = "Other - ASB",
                                                    "Do an act capable of encouraging/assisting suicide or attempt suicide of another" = "Other - Assisted suicide",
                                                    "Absconding from lawful custody" = "Other - Avoiding custody",
                                                    "Escape From Lawful Custody" = "Other - Avoiding custody",
                                                    "Remain unlawfully at large after recall to prison" = "Other - Avoiding recall",
                                                    "Temporarily released prisoner fails to comply to recall." = "	Other - Avoiding recall",
                                                    "Prohibition of activities without consent etc Human tissue act 2004 sec 32 abcde" = "Other - Biological",
                                                    "Make/supply/obtain articles for use in sect 1 or 3 offences  Computer Misuse Act" = "Other - Computing",
                                                    "Unauthorised act in relation to a computer causing / creating risk of serious damage" = "Other - Computing",
                                                    "Dishonestly obtaining electronic communication services" = "Other - Computing",
                                                    "Send/attempt to send false/misleading wireless teleg message endangering personal safety" = "Other - Computing",
                                                    "Corporate Manslaughter" = "Other - Corporate Manslaughter",
                                                    "Attempting to pervert the course of justice" = "Other - Dishonesty",
                                                    "Obstruction of officers  furnishing false information" = "Other - Dishonesty",
                                                    "Contempt of court by breach of sec 17 of the Criminal Procedure and Investgation Act 1996" = "Other - Dishonesty",
                                                    "False statements" = "Other - Dishonesty",
                                                    "Acknowledging bail in false name" = "Other - Dishonesty",
                                                    "Makes false statement or produce false evidence for the purpose of applying for a licence under Vehicles Excise Act" = "Other - Dishonesty",
                                                    "Disclosure Obstruction False or Misleading Statements etc" = "Other - Dishonesty",
                                                    "Identity Card Fraud" = "Other - Dishonesty",
                                                    "Other Offences Concealment of Birth" = "Other - Dishonesty",
                                                    "Disclosure  Statements  False statement to Officer" = "Other - Dishonesty",
                                                    "Domestic Incident" = "Other - Domestic",
                                                    "Unlawful eviction of occupier" = "Other - Domestic",
                                                    "Interference with a motor vehicle" = "Other - Driving",
                                                    "Driver injuring person by furious driving" = "Other - Driving",
                                                    "Tampering with motor vehicles" = "Other - Driving",
                                                    "Causing death by driving: unlicensed or uninsured drivers" = "Manslaughter",
                                                    "Vehicle Interference" = "Other - Driving",
                                                    "Causing death by careless or inconsiderate driving" = "Manslaughter",
                                                    "Causing death by careless driving while over prescribed limit - specified controlled drug" = "Manslaughter",
                                                    "Dangerous driving" = "Other - Driving",
                                                    "Wildlife Crime" = "Other - Environmental",
                                                    "Litter PND - Non Recordable CRI" = "Other - Environmental",
                                                    "Handling controlled waste without reasonable measure" = "Other - Environmental",
                                                    "Forced Marriage Offences" = "Other - Forced Marriage",
                                                    "Racist Incident (Non Recordable Crime)" = "Other - Hate",
                                                    "Use of words or behaviour or display of written material intended or likely to stir up racial hatred" = "Other - Hate",
                                                    "Hate Incident" = "Other - Hate",
                                                    "Terrorism" = "Other - Hate",
                                                    "Honour Based Abuse" = "Other - Hate",
                                                    "Homophobic Incident - Non Recordable Crime" = "Other - Hate",
                                                    "Religious Incident (Non Recordable Crime)" = "Other - Hate",
                                                    "Fail to disclose information about acts of terrorism" = "Other - Hate",
                                                    "With intent that self/to assist another to commit act of terrorism engage in preparation" = "Other - Hate",
                                                    "Publishing or distributing written material intended or likely to stir up racial hatred" = "Other - Hate",
                                                    "Offences regarding the disability discrimination act" = "Other - Hate",
                                                    "Public performance of a play intended or likely to stir up racial hatred" = "Other - Hate",
                                                    "Publish/cause another to publish a statement intending to/recklessly encourage terrorism" = "Other - Hate",
                                                    "Possess terrorist publication with view to distribution/sale/loan/read/listen to/seen etc" = "Other - Hate",
                                                    "Distribute/circulate a terrorist publication" = "Other - Hate",
                                                    "Transphobic Incident (Non Recordable Crime)" = "Other - Hate",
                                                    "Possession of racially inflammatory material" = "Other - Hate",
                                                    "Distributing showing or playing a recording intended or likely to stir up racial hatred" = "Other - Hate",
                                                    "Hunting" = "Other - Hunting",
                                                    "Hare Coursing" = "Other - Hunting",
                                                    "Triable either way offences under Immigration Act 2014" = "Other - Immigration",
                                                    "Unable to produce an immigration document at a leave or asylum interview in respect of a dependant child" = "Other - Immigration",
                                                    "Impersonation" = "Other - Impersonation",
                                                    "Committing or conspiring to commit an act outraging public decency" = "Other - Indenceny",
                                                    "Misconduct in Public Office Common Law" = "Other - Misconduct",
                                                    "Obstruct authorised person" = "Other - Obstruction of duty",
                                                    "Resisting or wilfully obstructing a designated or accredited person in the execution of their duty" = "Other - Obstruction of duty",
                                                    "Resists or wilfully obstructs a traffic officer in the execution of his dutie" = "Other - Obstruction of duty",
                                                    "Disclosure, Obstruction, False or Misleading Statements etc" = "Other - Obstruction of duty",
                                                    "Carrying on process without authority/non compliance with conditions of granted authority" = "Other - Obstruction of duty",
                                                    "Resisting or wilfully obstructing a prison custody officer or custody officer" = "Other - Obstruction of duty",
                                                    "Disclosure  Statements  Obstruction of Officer re warrant" = "Other - Obstruction of duty",
                                                    "Participate in the criminal activities of an organised crime group" = "Other - Organised Crime",
                                                    "Communicate false information alleging presence of bombs" = "Other - Pyrotecnics",
                                                    "Throwing Fireworks PND - Non Recordable CRI" = "Other - Pyrotecnics",
                                                    "Fireworks Regs Act (possession of adult firework by person under 18) PND - CRI" = "Other - Pyrotecnics",
                                                    "Bomb related incident" = "Other - Pyrotecnics",
                                                    "Placing or despatching articles to cause a bomb hoax" = "Other - Pyrotecnics",
                                                    "Making / having explosive substance under suspicious circumstances" = "Other - Pyrotecnics",
                                                    "Going equipped for stealing etc"	= "Other - Theft",
                                                    "Fish / take fish by other than licensable means in circumstances where fish / taking may or may not be authorised, or possess unlicensed instrument with intent to fish / take fish or without a S27A authority"	= "Other - Theft",
                                                    "Domestic Burglary (Attempted Distraction only) - Theft or criminal damage only"	= "Other - Theft",
                                                    "Trespassing on a Railway PND - Non Recordable CRI"	= "Other - Trespass",
                                                    "Unauthorised Encampment"	= "Other - Trespass",
                                                    "Rave"	= "Other - Trespass",
                                                    "Trespass with intent to commit a sexual offence"	= "Other - Trespass",
                                                    "Emergency COV19 Powers" = "Other - COVID19",
                                                    "Unexplained Death" = "Not a crime",
                                                    "Sudden Death" = "Not a crime",
                                                    "Section 136 Mental Health Act - non crime incident" = "Not a crime",
                                                    "S135 Mental Health Act â€“ Power of Entry" = "Not a crime",
                                                    "Other - Any Indictable Or TEW Offence Not Separately Classified" = "Other - Unclear",
                                                    "Disablist Incident (Non Recordable Crime)" = "Other - Unclear",
                                                    "Action Fraud - NFIB Referral" = "Other - Unclear",
                                                    "Notifiable Offences not classified elsewhere" = "Other - Unclear",
                                                    "Communications Act PND - CRI" = "Other - Unclear",
                                                    "Make / manufacture / possess an explosive / a thing / machine / engine / instrument / with intent to commit an offence" = "Other - Unclear",
                                                    "Offences Against Justice" = "Other - Unclear",
                                                    "Conveyance etc of List B articles into/out of a prison	Other" = "Other - Prison",
                                                    "Interfering with the mail postal operators" = "Acquisitive Offence",
                                                    "Without authority possess inside a prison an item specified in SEC 40 3B" = "Other - Prison",
                                                    "Conveyance etc of List A articles into/out of a prison" = "Other - Prison",
                                                    "Going Equipped" = "Other - Unclear",
                                                    "Suspected Cse - Non Crime Incident" = "Not a crime",
                                                    "Throwing Items over Prison Walls" = "Other - Prison",
                                                    "Indictable Offences Civil Partnership Act" = "Other - Unclear",
                                                    "Encourage/assist commision of either way offence Serious Crime Act" = "Other - Aiding and Abetting",
                                                    "Disclosure of information" = "Other - Unclear",
                                                    "Provision of postal services in contravention of restrictions" = "Acquisitive Offence",
                                                    "Other offences re prison security" = "Other - Prison",
                                                    "Encourage/asssist commision of indictable offence not murderSerious Crime Act" = "Other - Aiding and Abetting",
                                                    "Obtain/procure/disclose/retain personal data without consent of controller" = "Other - Unclear",
                                                    "Interfere with contractual relationships so as to harm animal research organisations" = "Other - Unclear",
                                                    "Failure to comply with regulations" = "Other - Unclear",
                                                    "TEW Offences in connection with information requirements Communictions Act 2003 excl s125 & s126" = "Other - Unclear",
                                                    "All TEW offences except S10, 78 to 82, 92 to 95 Railway Transport Safety Act 2003" = "Other - Unclear",
                                                    "Contravene/cause/permit another to breach Regs 358 or 9 Ecodesign for energy Regs 2010" = "Other - Environmental",
                                                    "Unlawful interception of postal public/private telecommunications  RIPA" = "Acquisitive Offence",
                                                    "Encourage/assist commision of indictable offence believing it will be committed not murder" = "Other - Aiding and Abetting",
                                                    "Wasting Police Time PND - CRI" = "Other - Wasting police time",
                                                    "Giving false alarm (fire and rescue) PND - CRI" = "Other - Wasting services time",
                                                    "Possess offensive weapon without lawful authority or reasonable excuse" = "Other - Weapon",
                                                    "Possessing or distributing prohibited weapons designed for discharge of noxious liquid etc" = "Other - Weapon",
                                                    "Offensive Weapon" = "Other - Weapon",
                                                    "Possession of Other Weapons" = "Other - Weapon",
                                                    "Possessing or distributing other prohibited weapons" = "Other - Weapon",
                                                    "Possession of firearm with intent to injure Group II" = "Other - Weapon",
                                                    "Assisting unlawful immigration to member state" = "Other- Immigration",
                                                    "Public Order: Harassment alarm or distress" = "Public Order",
                                                    "Public Order" = "Public Order",
                                                    "Other Offences against the State or Public Order" = "Public Order",
                                                    "Public Nuisance" = "Public Order",
                                                    "Racially and/or religiously aggravated harassment alarm or distress 9B" = "Public Order - Hate",
                                                    "Racially Aggravated Public Order Offences (Sec 31 Crime And Disorder Act)" = "Public Order - Hate",
                                                    "Rape of female over 16 years" = "Rape",
                                                    "N100/3 â€“ Reported Rape: Offence committed in another police force area/outside the UK" = "Rape",
                                                    "N100/1 - Reported Rape: Victim (or third party acting on their behalf)  has not confirmed the offence or cannot be traced" = "Not a crime",
                                                    "N100/2 - Reported Rape: Credible evidence to the contrary exists" = "Not a crime",
                                                    "Rape of a Female aged 16 and over" = "Rape",
                                                    "Cause a male person to engage in sexual activity without consent  penetration" = "Rape",
                                                    "Rape" = "Rape",
                                                    "Rape of male 16 years or over" = "Rape",
                                                    "Sex act with female person with mental disorder impeding choice  penetration" = "Rape",
                                                    "Cause a female person to engage in sexual activity without consent  penetration" = "Rape",
                                                    "Committing an offence with intent to commit a sexual offence" = "Sexual Offence",
                                                    "Rape (Multiple Undefined Offenders) of female under 16 years" = "Rape",
                                                    "Attempted rape of male 16 years or over" = "Rape",
                                                    "Cause/incite female with mental disorder impeding choice to sex activity  penetration" = "Rape",
                                                    "Attempted rape (Multiple Undefined Offenders) of male 16 years or over" = "Rape",
                                                    "Incite female relative under 13 to sex activity  penetration Off under 18" = "Rape - Child",
                                                    "Sex activity with a female family member 13 17yrs  penetration  offender 18" = "Rape - Child",
                                                    "Rape (Multiple Undefined Offenders) of female over 16 years" = "Rape - Child",
                                                    "Paying for the sexual services of a female child 1617yrs" = "Rape - Child",
                                                    "Cause/incite male child under 16 to sex activity  penetration  offender under 18" = "Rape - Child",
                                                    "Rape (Multiple Undefined Offenders) of a female child under 13 by a male" = "Rape - Child",
                                                    "Cause/incite male child under 13 to sex act  penetration  offender under 18" = "Rape - Child",
                                                    "Paying for the sexual services of a female child under 16  penetration" = "Rape - Child",
                                                    "Rape of a Female Child under 13" = "Rape - Child",
                                                    "Robbery - Personal" = "Robbery",
                                                    "Robbery - Business" = "Robbery",
                                                    "Assault with intent to Rob - Personal" = "Robbery",
                                                    "Robbery" = "Robbery",
                                                    "Robbery Of Personal Property" = "Robbery",
                                                    "Assault wth intent to Rob - Business" = "Robbery",
                                                    "Disclose private sexual photographs and films with intent to cause distress" = "Sexual Offence",
                                                    "Sexual Activity" = "Sexual Offence",
                                                    "Sexual Assault" = "Sexual Offence",
                                                    "Cause a male person to engage in sexual activity without consent  no penetration" = "Sexual Offence",
                                                    "Breach SHPO / interim SHPO /SOPO / interim SOPO/ Foreign travel order or fail to comply with a requirement Under Sec 103D (4)" = "Sexual Offence",
                                                    "Exposureintentionalmale or female genitals" = "Sexual Offence",
                                                    "Possess an extreme image portraying sexual assault by penetration" = "Sexual Offence",
                                                    "Obscene Publications Etc." = "Sexual Offence",
                                                    "Possess extreme pgraphic image of person having icourse/oral sex with animal  alive/dead" = "Sexual Offence",
                                                    "Exposure and Voyeurism" = "Sexual Offence",
                                                    "Indecent matter publicly displayed" = "Sexual Offence",
                                                    "Breach a sexual risk order / risk of sexual harm order etc or fail to comply with requirement under Sec 122 c (4)" = "Sexual Offence",
                                                    "Arrange/facilitate the commission of a child sex offence" = "Sexual Offence",
                                                    "Cause a female person to engage in sexual activity without consent  no penetration" = "Sexual Offence",
                                                    "Voyeurism Observing/Op or installing equipt/recording  private act for sex gratificn" = "Sexual Offence",
                                                    "Bigamy" = "Sexual Offence",
                                                    "Keeping a brothel used for prostitution" = "Sexual Offence",
                                                    "Cause/incite prostitution for gain" = "Sexual Offence",
                                                    "Possess extreme pornographic Image depicting act threatening a persons life" = "Sexual Offence",
                                                    "Cause/incite male with mental disorder impeding choice to sex activity  no penetration" = "Sexual Offence",
                                                    "Possessing obscene material for gain" = "Sexual Offence",
                                                    "Intercourse with an animal by a male" = "Sexual Offence",
                                                    "Possess extreme pgraphic image of act likely to seriously injure anus/breast/genitals" = "Sexual Offence",
                                                    "Solicit another for obtaining sexual services as a prostitute in street or public place" = "Sexual Offence",
                                                    "Possess an extreme image portraying rape" = "Sexual Offence",
                                                    "Care worker causing/inciting sexual activity with person with mental disorder  no penetration" = "Sexual Offence",
                                                    "Voyeurism (Up-skirting)" = "Sexual Offence",
                                                    "Cause/incite female with mental disorder impeding choice to sex activity  no penetrn" = "Sexual Offence",
                                                    "Cause person with mental disorder to sex act by inducemt/threat/deceptn  no penetrn" = "Sexual Offence",
                                                    "Inducemt/threat/deceptn to procure sex act with mentally disorded  no penetrn" = "Sexual Offence",
                                                    "Care worker  Sex act with female person with mental disorder  penetration" = "Sexual Offence",
                                                    "Take/make/publish indecent photo/pseudophoto of a child" = "Sexual Offence - Child",
                                                    "Sexual assault on a female 13 or over" = "Sexual Offence - Child",
                                                    "Possession of indecent photo of child" = "Sexual Offence - Child",
                                                    "Sexual assault on a female aged 13 and over by penetration" = "Sexual Offence - Child",
                                                    "Sex activity with a female child under 16  penetration  offender 18" = "Sexual Offence - Child",
                                                    "Sex activity with a female child under 16  penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Sexual assault of a female child under 13" = "Sexual Offence - Child",
                                                    "Cause/incite female child under 13 to sex actpenetrationoffender any age" = "Sexual Offence - Child",
                                                    "Attempted rape of a female16 years or over" = "Sexual Offence - Child",
                                                    "Rape of female under 16 years" = "Sexual Offence - Child",
                                                    "Sexual Assault on a Male aged 13 and over - Non Penetration" = "Sexual Offence - Child",
                                                    "Possess prohibited obscene images of children eg cartoon images etc s  C  CJ Act" = "Sexual Offence - Child",
                                                    "Rape of a female child under 13 by a male" = "Sexual Offence - Child",
                                                    "Sexual assault of a female child under 13 by penetration" = "Sexual Offence - Child",
                                                    "Cause a child under 16 to watch a sex act  offender under 18" = "Sexual Offence - Child",
                                                    "Sexual assault on a male child under 13" = "Sexual Offence - Child",
                                                    "Cause/incite female child under 16 to sex activity  penetration  offender 18" = "Sexual Offence - Child",
                                                    "Sex activity with a male child under 16  penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Sex activity with a male child under 16  no penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Rape of male under 16 years" = "Sexual Offence - Child",
                                                    "Cause/incite female child under 13 to sex act  penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Cause/incite child prostitution or pornography  child aged 13  17" = "Sexual Offence - Child",
                                                    "Cause/incite female child under 16 to sex act  no penetration  offender 18" = "Sexual Offence - Child",
                                                    "Sex activity with a female child under 13  no penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Cause/incite male child under 13 to sex act  no penetration  offender any age" = "Sexual Offence - Child",
                                                    "Cause/incite female under 16 to sex activity  no penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Sexual Assault of a male child under 13 by penetration" = "Sexual Offence - Child",
                                                    "Cause/incite male child under 16 to sex act  no penetration  offender 18" = "Sexual Offence - Child",
                                                    "Sex activity with a female child under 16  no penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Cause/incite female child under 13 to sex act  no penetration  offender any age" = "Sexual Offence - Child",
                                                    "Rape of a male child under 13 by a male" = "Sexual Offence - Child",
                                                    "Engage in sexual communication with a child" = "Sexual Offence - Child",
                                                    "Other Miscellaneous Sexual Offences" = "Sexual Offence - Child",
                                                    "Cause/incite female child under 16 to sex activity  penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Cause/incite female child under 13 to sex activity  no penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Sex activity with male family member 13  17yrs  no penetn  Off under 18" = "Sexual Offence - Child",
                                                    "Cause a child under 16 to watch a sex act  offender 18" = "Sexual Offence - Child",
                                                    "Cause a child under 13 to watch a sexual act  offender 18" = "Sexual Offence - Child",
                                                    "Cause/incite male child under 13 to sex activity  no penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Attempted rape of a female child under 13 by a male" = "Sexual Offence - Child",
                                                    "Sexual Assault on a Male aged 13 and over - Penetration" = "Sexual Offence - Child",
                                                    "Cause/incite male child under 16 to sex activity  no penetration  off under 18" = "Sexual Offence - Child",
                                                    "Attempted rape of a male child under 13 by a male" = "Sexual Offence - Child",
                                                    "Sex activity with female child under 16  no penetration  offender 18" = "Sexual Offence - Child",
                                                    "Cause a child under 13 to watch a sex act  offender under 18" = "Sexual Offence - Child",
                                                    "Meeting a male child under 16 following sexual grooming etc  offender 18" = "Sexual Offence - Child",
                                                    "Engage in sex activity in the presence of a child under 13  offender 18" = "Sexual Offence - Child",
                                                    "Sex activity with male relative 13 17yrs  no penetration Offender 18" = "Sexual Offence - Child",
                                                    "Sex activity with a male child under 13  no penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Sexual Grooming" = "Sexual Offence - Child",
                                                    "Sexual Activity involving a Child under 16" = "Sexual Offence - Child",
                                                    "Sexual Assault on a Male aged 13 and over" = "Sexual Offence - Child",
                                                    "Attempted rape of a female under 16 years" = "Sexual Offence - Child",
                                                    "Sex activity with a male child under 16  penetration  offender 18" = "Sexual Offence - Child",
                                                    "Sexual Assault on a Female aged 13 and over" = "Sexual Offence - Child",
                                                    "Meeting a female child under 16 following sexual grooming etc  offender 18" = "Sexual Offence - Child",
                                                    "Rape of a Female Child under 16" = "Sexual Offence - Child",
                                                    "Child Abduction" = "Sexual Offence - Child",
                                                    "Sex activity with female child under 13  penetration  offender under 18" = "Sexual Offence - Child",
                                                    "Sex activity with a male child under 16  no penetration  offender 18" = "Sexual Offence - Child",
                                                    "Abuse position of trust Sex activity with female 13  17yrs Offender 18" = "Sexual Offence - Child",
                                                    "Abuse position of trust Sex activity with male 13  17yrs Offender 18" = "Sexual Offence - Child",
                                                    "Engage in sex activity in presence of child under 13  offender under 18" = "Sexual Offence - Child",
                                                    "Cause/incite male child under 16 to sex activity  penetration  offender 18" = "Sexual Offence - Child",
                                                    "Arrange/facilitate child prostitution or pornography  child aged 13  17" = "Sexual Offence - Child",
                                                    "Sexual Assault on a Female Child under 13" = "Sexual Offence - Child",
                                                    "Sexual Activity involving a Child under 13" = "Sexual Offence - Child",
                                                    "Engage in sex activity in the presence of child under 16  offender under 18" = "Sexual Offence - Child",
                                                    "Cause/incite male child under 13 to sex act  penetration  offender any age" = "Sexual Offence - Child",
                                                    "Abuse posn of trust Sex activity with female under 13yrs Offender 18" = "Sexual Offence - Child",
                                                    "Incite female relative under 13 to sex activity  no penetn Off18" = "Sexual Offence - Child",
                                                    "Abuse posn of trust Cause/incite female 13  17yrs to sex activity Offender 18" = "Sexual Offence - Child",
                                                    "Incite male relative under 13 to sex activity no penetration Off under 18" = "Sexual Offence - Child",
                                                    "Incite female family member 1317yrs to sex activity  no penetn  Offunder 18" = "Sexual Offence - Child",
                                                    "Abuse posn of trust Sex activity in presence of male or female 13  17yrs Off 18" = "Sexual Offence - Child",
                                                    "Sex activity with female relative under 13  no penetration Offender 18" = "Sexual Offence - Child",
                                                    "Incite male relative under 13 to sex activity  no penetn Offender 18" = "Sexual Offence - Child",
                                                    "Abuse posn of trust Cause/incite male 13  17yrs to sex activity Offender 18" = "Sexual Offence - Child",
                                                    "Sex activity with male relative under 13  no penetration Offender 18" = "Sexual Offence - Child",
                                                    "Engage in sex activity in the presence of child under 16  offender 18" = "Sexual Offence - Child",
                                                    "Cause/incite child prostitution or pornography  child under 13" = "Sexual Offence - Child",
                                                    "Incite male relative 13  17 yrs to sex activity  no penetration Off 18" = "Sexual Offence - Child",
                                                    "Inciting female relative 13  17yrs to sex activity  no penetration Off 18" = "Sexual Offence - Child",
                                                    "Paying for the sexual services of a female child under 16  no penetration" = "Sexual Offence - Child",
                                                    "Drugs wef 26/01/09 Possess cannabis class B wintent to supply" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  Other class B" = "Supply of Drugs",
                                                    "Drugs wef 26/1/09 Supply/offer to supply cannabis class B" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  Other class A" = "Supply of Drugs",
                                                    "Supply or offering to supply a controlled drug  Other class A" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  Heroin" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  Crack" = "Supply of Drugs",
                                                    "Supply or offering to supply a controlled drug  Heroin" = "Supply of Drugs",
                                                    "Supply or offering to supply a controlled drug  cocaine" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  Other class C" = "Supply of Drugs",
                                                    "Drugs Pre 26/1/09 Possess cannabis class C on ship  trafficking" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  Cocaine" = "Supply of Drugs",
                                                    "Drugs wef 26/1/09 Production of cannabis class B" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  unspecified" = "Supply of Drugs",
                                                    "Production or being concerned in production of controlled drug  other class B" = "Supply of Drugs",
                                                    "Unlawful importation of a drug controlled under misuse of drugs act  class A" = "Supply of Drugs",
                                                    "Supply or offering to supply a controlled drug  unspecified" = "Supply of Drugs",
                                                    "Other drugs offences" = "Supply of Drugs",
                                                    "Obstructing a PC in exercise of a Sec 23A 6 power to detain or search a person vehicle or vessel regarding a drug the subject of a temporary class drug order" =	"Supply of Drugs",
                                                    "Conceal or transfer proceeds of drugs trafficking" = "Supply of Drugs",
                                                    "Supply or offering to supply a controlled drug  Other class C" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  MDMA" = "Supply of Drugs",
                                                    "Production or being concerned in production of controlled drug  cocaine" = "Supply of Drugs",
                                                    "Obstruct powers of search or concealing drugs" = "Supply of Drugs",
                                                    "Production or being concerned in production of controlled drug  Other class A" = "Supply of Drugs",
                                                    "Supply or offering to supply a controlled drug  LSD" = "Supply of Drugs",
                                                    "Unlawful importation of a drug controlled under misuse of drugs act  class B" = "Supply of Drugs",
                                                    "Supply or offering to supply a controlled drug  Other class B" = "Supply of Drugs",
                                                    "Class B Offer/concerned in/supply Cathinone derivative incMephedrone" = "Supply of Drugs",
                                                    "Supply/offer/concerned in supply of controlled drug  Synthetic Cannabinoid" = "Supply of Drugs",
                                                    "Drug Supplying (Incl. Possession W/I To Supply)/Production/Cultivation" = "Supply of Drugs",
                                                    "Supply/offender/concerned in offer to supply a controlled drug  GBL/14BD" = "Supply of Drugs",
                                                    "Proceeds Of Crime Offences" = "Supply of Drugs",
                                                    "Manufacturing a scheduled substance" = "Supply of Drugs",
                                                    "Possession of a controlled drug w/intent to supply  Synthetic Cannabinoid" = "Supply of Drugs",
                                                    "Supply or offering to supply a controlled drug  Crack" = "Supply of Drugs",
                                                    "Class B Possession w/intent to supply Cathinone derivative incl Mephedrone" = "Supply of Drugs",
                                                    "Production or being concerned in production of controlled drug  unspecified" = "Supply of Drugs",
                                                    "Unlawful importation of a drug controlled under misuse of drugs act  class C" = "Supply of Drugs",
                                                    "Supply or being concerned in the supply of a drug subject of a temporary class drug order" = "Supply of Drugs",
                                                    "Acquisition possession or use of proceeds of drug trafficking" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  Ketamine" = "Supply of Drugs",
                                                    "Possess a psychoactive substance with intent to supply" = "Supply of Drugs",
                                                    "Import a psychoactive substance" = "Supply of Drugs",
                                                    "Production/concerned in production of a controlled drug  Synthetic Cannabinoid" = "Supply of Drugs",
                                                    "Class B Concerned in/Production of Cathinone derivative incl Mephedrone" = "Supply of Drugs",
                                                    "Carrying or concealing on a ship controlled drug intended for trafficking class unspecified" = "Supply of Drugs",
                                                    "Drugs wef 26/1/09 Permit premises to be used for unlawful purpose  cannabis class B" = "Supply of Drugs",
                                                    "Supply a psychoactive substance" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  LSD" = "Supply of Drugs",
                                                    "Supplying a scheduled substance to another person" = "Supply of Drugs",
                                                    "Incite another to supply class A drug" = "Supply of Drugs",
                                                    "Production or being concerned in production of a controlled drug Ketamine" = "Supply of Drugs",
                                                    "Possession of a controlled drug with intent to supply  Amphetamine" = "Supply of Drugs",
                                                    "Supply intoxicating substance" = "Supply of Drugs",
                                                    "Drink / Drug Drive" = "Under the influence",
                                                    "Drunk and Disorderly PND - CRI" = "Under the influence",
                                                    "Act as crew of aircraft when under influence of drink/drug impairing capacity" = "Under the influence",
                                                    "Drunk in a Highway PND - CRI" = "Under the influence",
                                                    "Death by Driving: Unlicensed,Disqualified or Uninsured" = "Under the influence",
                                                    "null" = "Not a crime")

# Categorise each offence type into numeric form:

Personal_Occurence_DF_Descriptive$Offence <- ifelse(Personal_Occurence_DF_Descriptive$Offence == "Assault and Battery" |
                                                      Personal_Occurence_DF_Descriptive$Offence == "Assault and Battery - Hate" |
                                                      Personal_Occurence_DF_Descriptive$Offence == "Assault and Battery (Public Order - Hate)" |
                                                      Personal_Occurence_DF_Descriptive$Offence == "Assault and Battery (Public Order)" |
                                                      Personal_Occurence_DF_Descriptive$Offence == "Assault and Battery with a weapon", "1",
                                                    
                                                    ifelse(Personal_Occurence_DF_Descriptive$Offence == "ABH", "2",
                                                           
                                                           ifelse(Personal_Occurence_DF_Descriptive$Offence == "GBH" |
                                                                    Personal_Occurence_DF_Descriptive$Offence == "GBH and more serious violence" |
                                                                    Personal_Occurence_DF_Descriptive$Offence == "GBH and more serious violence with weapon", "3", 
                                                                  
                                                                  ifelse(Personal_Occurence_DF_Descriptive$Offence == "Murder" | 
                                                                           Personal_Occurence_DF_Descriptive$Offence == "Attempted Murder" | 
                                                                           Personal_Occurence_DF_Descriptive$Offence == "Manslaughter", "4",
                                                                         
                                                                         ifelse(Personal_Occurence_DF_Descriptive$Offence == "Robbery" | 
                                                                                  Personal_Occurence_DF_Descriptive$Offence == "Other - Violence", "5",
                                                                                
                                                                                ifelse(Personal_Occurence_DF_Descriptive$Offence == "Sexual Offence" |
                                                                                         Personal_Occurence_DF_Descriptive$Offence == "Incest" |
                                                                                         Personal_Occurence_DF_Descriptive$Offence == "Sexual Offence - Child", "6",
                                                                                       
                                                                                       ifelse(Personal_Occurence_DF_Descriptive$Offence == "Rape" |
                                                                                                Personal_Occurence_DF_Descriptive$Offence == "Rape - Child", "7",
                                                                                              
                                                                                              ifelse(Personal_Occurence_DF_Descriptive$Offence == "Neglect", "8",
                                                                                                     
                                                                                                     ifelse(Personal_Occurence_DF_Descriptive$Offence == "Theft" |
                                                                                                              Personal_Occurence_DF_Descriptive$Offence ==  "Acquisitive Offence" |
                                                                                                              Personal_Occurence_DF_Descriptive$Offence == "Burglary" |
                                                                                                              Personal_Occurence_DF_Descriptive$Offence == "Other - Theft" |
                                                                                                              Personal_Occurence_DF_Descriptive$Offence == "Financial Crime", "9",
                                                                                                            
                                                                                                            ifelse(Personal_Occurence_DF_Descriptive$Offence == "Property Damage" |
                                                                                                                     Personal_Occurence_DF_Descriptive$Offence == "Criminal Damage", "10",
                                                                                                                   
                                                                                                                   ifelse(Personal_Occurence_DF_Descriptive$Offence == "Exploitation and Trafficking", "11",
                                                                                                                          
                                                                                                                          ifelse(Personal_Occurence_DF_Descriptive$Offence == "Firearm - Possession" |
                                                                                                                                   Personal_Occurence_DF_Descriptive$Offence == "Knife - Possession" |
                                                                                                                                   Personal_Occurence_DF_Descriptive$Offence == "Other - Weapon", "12",
                                                                                                                                 
                                                                                                                                 ifelse(Personal_Occurence_DF_Descriptive$Offence == "Harrassment" |
                                                                                                                                          Personal_Occurence_DF_Descriptive$Offence == "Breach of Order - Harrassment", "13",
                                                                                                                                        
                                                                                                                                        ifelse(Personal_Occurence_DF_Descriptive$Offence == "Drugs related" |
                                                                                                                                                 Personal_Occurence_DF_Descriptive$Offence == "Supply of Drugs", "14",
                                                                                                                                               
                                                                                                                                               ifelse(Personal_Occurence_DF_Descriptive$Offence == "Public Order" |
                                                                                                                                                        Personal_Occurence_DF_Descriptive$Offence == "Public Order - Hate", "15",
                                                                                                                                                      
                                                                                                                                                      ifelse(Personal_Occurence_DF_Descriptive$Offence == "Other - Aiding and Abetting" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Aircraft" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Alcohol" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - ASB" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Assisted suicide" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Avoiding custody" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Avoiding recall" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Biological" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Computing" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Corporate Manslaughter" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Dishonesty" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Domestic" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Domicile" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Driving" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Environmental" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Forced Marriage" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Fraud" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Hate" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Hunting" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Immigration" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Impersonation" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Indenceny" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Misconduct" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Obstruction of duty" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Organised Crime" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Pyrotecnics" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Trespass" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Unclear" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Breach of Order - Other" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Wasting police time" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other - Wasting services time" |
                                                                                                                                                               Personal_Occurence_DF_Descriptive$Offence == "Other- Immigration", "16", 
                                                                                                                                                             ifelse(Personal_Occurence_DF_Descriptive$Offence == "No Crime" |
                                                                                                                                                                      Personal_Occurence_DF_Descriptive$Offence == "Not a crime", "17","0")))))))))))))))))


# Recode Outcomes into logical and loose definition of an offence committed, let 10 show that it is considered that an offence has not taken place:

Personal_Occurence_DF_Descriptive$Outcome <- recode(Personal_Occurence_DF_Descriptive$Outcome,
                                                    "New" = "10",
                                                    "NB no crime" = "10",
                                                    "F1 street warning - cannabis possession" = "2",
                                                    "E1 Fixed penalty notice - alternate offence rule" = "2",
                                                    "E1 Fixed penalty notice" = "2",
                                                    "CRI6 created for custody disposal - Scrutineer use only" = "10",
                                                    "CRI5 credible evidence to the contrary" = "10",
                                                    "CRI4 NCRS/HOCR directs that a crime should not be recorded" = "10",
                                                    "CRI3 incident being dealt with by another force" = "10",
                                                    "CRI2 3rd party report - alleged victim cannot be traced" = "2",
                                                    "CRI1 3rd party report - alleged victim declines to confirm crime" = "7",
                                                    "CMR2 Youth Restorative disposal" = "4",
                                                    "CMR1 Adult Restorative disposal" = "4",
                                                    "C2 TIC not previously recorded" = "4",
                                                    "C1 TIC recorded" = "4",
                                                    "B6 youth conditional caution - alternate offence rule" = "3",
                                                    "B6 youth conditional caution" = "3",
                                                    "B5 youth caution - alternate offence rule" = "3",
                                                    "B5 youth caution" = "3",
                                                    "B2 Adult conditional caution - alternate offence rule" = "3",
                                                    "B2 Adult conditional caution" = "3",
                                                    "B1 Adult simple caution - alternate offence rule" = "3",
                                                    "B1 Adult simple caution" = "3",
                                                    "A3 charged - alternate offence rule" = "5",
                                                    "A2 summonsed - alternate offence rule" = "5",
                                                    "A2 summonsed" = "5",
                                                    "A1 charged" = "5",
                                                    "9 - Prosecution not in the Public Interest - CPS" = "9",
                                                    "5 - Offender Dead - All Offences" = "9",
                                                    "22 - Diversionary, educational or intervention activity, not in public interest to take further action" = "9",
                                                    "21 - Investigation supports action against suspect - Police decide not in public interest" = "9",
                                                    "20 - Other Agency dealing" = "10",
                                                    "18 - No Suspect - Investigation Complete. Filed pending further Information" = "10",
                                                    "17 - Suspect Identified - Prosecution Time Limit Expired" = "6",
                                                    "16 - Suspect Identified - Evidential Difficulties - Victim not supporting" = "7",
                                                    "15 - Suspect Identified - Evidential Difficulties - Victim supports action" = "6",
                                                    "14 - Suspect not identified - Victim Declines/Unable to identify suspect" = "7",
                                                    "13 - Suspect Identified - Victim/Key Witness dead/too ill to give evidence" = "8",
                                                    "12 - Suspect Identified - Too ill (physical/mental health) to Prosecute" = "9",
                                                    "11 - Suspect Identified - Below Age of Criminal Responsibility" = "9",
                                                    "10 - Formal Action against suspect not in the Public Interest - Police" = "9")


#  Deal with na values identified in the code and respecify variables where pertinent to do so:

Personal_Occurence_DF_Descriptive$Offence <- Personal_Occurence_DF_Descriptive$Offence %>% replace(is.na(.), 0)
Personal_Occurence_DF_Descriptive$Gender <- Personal_Occurence_DF_Descriptive$Gender %>% replace(is.na(.), 1) 
Personal_Occurence_DF_Descriptive$Ethnicity[is.null(Personal_Occurence_DF_Descriptive$Ethnicity)] <- 0

Personal_Occurence_DF_Descriptive$Age_cohort <- factor(Personal_Occurence_DF_Descriptive$Age_cohort)
Personal_Occurence_DF_Descriptive$Age_at_incident <- as.integer(Personal_Occurence_DF_Descriptive$Age_at_incident)
Personal_Occurence_DF_Descriptive$Gender <- factor(Personal_Occurence_DF_Descriptive$Gender)
Personal_Occurence_DF_Descriptive$Original.Gender <- factor(Personal_Occurence_DF_Descriptive$Original.Gender)
Personal_Occurence_DF_Descriptive$Ethnicity <- factor(Personal_Occurence_DF_Descriptive$Ethnicity)
Personal_Occurence_DF_Descriptive$Victim_status <- factor(Personal_Occurence_DF_Descriptive$Victim_status)
Personal_Occurence_DF_Descriptive$Witness <- factor(Personal_Occurence_DF_Descriptive$Witness)
Personal_Occurence_DF_Descriptive$Injured_party <- factor(Personal_Occurence_DF_Descriptive$Injured_party)
Personal_Occurence_DF_Descriptive$Eliminated_from_investigation <- factor(Personal_Occurence_DF_Descriptive$Eliminated_from_investigation)
Personal_Occurence_DF_Descriptive$Suspect <- factor(Personal_Occurence_DF_Descriptive$Suspect)
Personal_Occurence_DF_Descriptive$Offence <- factor(Personal_Occurence_DF_Descriptive$Offence)
Personal_Occurence_DF_Descriptive$Outcome <- factor(Personal_Occurence_DF_Descriptive$Outcome)
str(Personal_Occurence_DF_Descriptive)




# Take unique count of males, females and unknown in the cohort - this indicates how many are missing information:

unique_counts <- Personal_Occurence_DF_Descriptive %>%
  distinct(Person_ID, .keep_all = TRUE) %>%
  count(Gender)

unique_counts_table_Gender <- unique_counts %>%
  gt() %>%
  tab_header(
    title = "Unique Counts by Gender"
  ) %>%
  cols_label(
    Gender = "Gender",
    n = "Count"
  ) %>%
  tab_options(
    table.font.size = 12 
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Times New Roman")
    ),
    locations = cells_body()
  )

unique_counts_table_Gender



# Identify the offences committed by the individuals with an unknown Gender, this aids in defending their subsequent deletion from the dataset

unknown_gender_offences <- Personal_Occurence_DF_Descriptive %>%
  filter(Gender == 1,  
         Victim_status == 0,  
         Witness == 0,  
         Eliminated_from_investigation == 0)  

offence_table <- unknown_gender_offences %>%
  group_by(Offence) %>%
  summarise(count = n()) %>%
  arrange(desc(Offence))  

print(offence_table)



# Take unique counts of number of individuals that are classified by specific ethnicities in the cohort:

unique_counts <- Personal_Occurence_DF_Descriptive %>%
  distinct(Person_ID, .keep_all = TRUE) %>%
  count(Ethnicity)

unique_counts_table_Ethnicity <- unique_counts %>%
  gt() %>%
  tab_header(
    title = "Unique Counts by Ethnicity"
  ) %>%
  cols_label(
    Ethnicity = "Ethnicity",
    n = "Count"
  ) %>%
  tab_options(
    table.font.size = 12 
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Times New Roman")
    ),
    locations = cells_body()
  )

unique_counts_table_Ethnicity



# Listwise deletion for Unknown Gender:

Personal_Occurence_DF_Descriptive <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Gender != 0 & Personal_Occurence_DF_Descriptive$Gender != 1, ]

n_distinct(Personal_Occurence_DF_Descriptive$Gender)


# Final Count by Gender and Ethnicity after Listwise Deletion

# Gender

unique_counts <- Personal_Occurence_DF_Descriptive %>%
  distinct(Person_ID, .keep_all = TRUE) %>%
  count(Gender)

unique_counts_table_Gender <- unique_counts %>%
  gt() %>%
  tab_header(
    title = "Unique Counts by Gender"
  ) %>%
  cols_label(
    Gender = "Gender",
    n = "Count"
  ) %>%
  tab_options(
    table.font.size = 12 
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Times New Roman")
    ),
    locations = cells_body()
  )

unique_counts_table_Gender


# Ethnicity

unique_counts <- Personal_Occurence_DF_Descriptive %>%
  distinct(Person_ID, .keep_all = TRUE) %>%
  count(Ethnicity)

unique_counts_table_Ethnicity <- unique_counts %>%
  gt() %>%
  tab_header(
    title = "Unique Counts by Ethnicity"
  ) %>%
  cols_label(
    Ethnicity = "Ethnicity",
    n = "Count"
  ) %>%
  tab_options(
    table.font.size = 12 
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Times New Roman")
    ),
    locations = cells_body()
  )

unique_counts_table_Ethnicity



# De-duplicate identical entries for Missing Persons (multiple duplicates exist upon eyeball inspection):

unique_missing_incidents <- Personal_Occurence_DF_Descriptive %>%
  filter(Missing_Incident == "Missing Incident") %>%
  distinct(Person_ID, Time_of_incident, .keep_all = TRUE)

non_missing_incidents <- Personal_Occurence_DF_Descriptive %>%
  filter(Missing_Incident != "Missing Incident")

Personal_Occurence_DF_Descriptive <- bind_rows(unique_missing_incidents, non_missing_incidents)

Personal_Occurence_DF_Descriptive <- Personal_Occurence_DF_Descriptive %>% filter(!is.na(Age_at_incident))



# Count of number of individuals who have been missing:

unique_missing_counts <- Personal_Occurence_DF_Descriptive %>%
  filter(Missing_Incident == "Missing Incident") %>%
  distinct(Person_ID, .keep_all = TRUE) %>%
  count(Missing_Incident)

unique_counts_table_missing <- unique_missing_counts %>%
  gt() %>%
  tab_header(
    title = "Unique Counts of Persons with Missing Incidents"
  ) %>%
  cols_label(
    Missing_Incident = "Missing Incident",
    n = "Count"
  ) %>%
  tab_options(
    table.font.size = 12
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Times New Roman")
    ),
    locations = cells_body()
  )

unique_counts_table_missing



# Identify only those who have a missing incident and no other incident:

only_missing_individuals <- Personal_Occurence_DF_Descriptive %>%
  group_by(Person_ID, Gender) %>%
  filter(all(Missing_Incident == "Missing Incident")) %>%  
  distinct(Person_ID, Gender) %>%                          
  count(Gender)                                             
summary_table <- only_missing_individuals %>%
  group_by(Gender) %>%
  summarise(Total = sum(n), .groups = 'drop')  

overall_count <- nrow(only_missing_individuals)

cat("Number of individuals with only Missing Incident:", overall_count, "\n")

summary_table_missing_only <- summary_table %>%
  gt() %>%
  tab_header(
    title = "Total Count of Individuals with Only Missing Incidents by Gender"
  ) %>%
  cols_label(
    Gender = "Gender",
    Total = "Total Individuals"
  ) %>%
  tab_options(
    table.font.size = 12
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Times New Roman") 
    ),
    locations = cells_body()
  )

summary_table_missing_only


# Filter out eliminated party in population dataframe as this was discussed with TVP and they are equal to innocent. This effectively also removes all entries of data subjects with an outcome = 10

Personal_Occurence_DF_Descriptive <- subset(Personal_Occurence_DF_Descriptive, Personal_Occurence_DF_Descriptive$Eliminated_from_investigation != 1)
n_distinct(Personal_Occurence_DF_Descriptive$Person_ID)


# Unique counts by Gender and Ethnicity of new dataframe

# Gender

unique_counts <- Personal_Occurence_DF_Descriptive %>%
  distinct(Person_ID, .keep_all = TRUE) %>%
  count(Gender)

unique_counts_table_Gender <- unique_counts %>%
  gt() %>%
  tab_header(
    title = "Unique Counts by Gender"
  ) %>%
  cols_label(
    Gender = "Gender",
    n = "Count"
  ) %>%
  tab_options(
    table.font.size = 12 
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Times New Roman")
    ),
    locations = cells_body()
  )

unique_counts_table_Gender



# Ethnicity

unique_counts <- Personal_Occurence_DF_Descriptive %>%
  distinct(Person_ID, .keep_all = TRUE) %>%
  count(Ethnicity)

unique_counts_table_Ethnicity <- unique_counts %>%
  gt() %>%
  tab_header(
    title = "Unique Counts by Ethnicity"
  ) %>%
  cols_label(
    Ethnicity = "Ethnicity",
    n = "Count"
  ) %>%
  tab_options(
    table.font.size = 12 
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "Times New Roman")
    ),
    locations = cells_body()
  )

unique_counts_table_Ethnicity


# 25 Establish timing of incident per the time of day - this will be used in the descriptives


Personal_Occurence_DF_Descriptive <- Personal_Occurence_DF_Descriptive %>%
  mutate(Time_of_Day = ymd_hms(Time_of_Day)) %>%  
  mutate(hour = hour(Time_of_Day)) %>%  
  mutate(Time_of_Day_label = case_when(
    hour >= 5 & hour <= 11 ~ "1",    # 0500 to 1159
    hour >= 12 & hour <= 16 ~ "2", # 1200 to 1659
    hour >= 17 & hour <= 20 ~ "3",   # 1700 to 2059
    TRUE ~ "4"  # 2100 to 0459
  ))



# 26 Establish Season of incident - this will be used in the descriptives


Personal_Occurence_DF_Descriptive$Time_of_Day <- as.POSIXct(Personal_Occurence_DF_Descriptive$Time_of_Day, format = "%Y-%m-%dT%H:%M:%OSZ")
Personal_Occurence_DF_Descriptive$Month <- as.numeric(format(Personal_Occurence_DF_Descriptive$Time_of_Day, "%m"))

Personal_Occurence_DF_Descriptive$Season <- ifelse(Personal_Occurence_DF_Descriptive$Month %in% 9:11, "1",       # September, October, November
                                                   ifelse(Personal_Occurence_DF_Descriptive$Month %in% 12 | Personal_Occurence_DF_Descriptive$Month %in% 1:2, "2",  # December, January, February
                                                          ifelse(Personal_Occurence_DF_Descriptive$Month %in% 3:5, "3",       # March, April, May
                                                                 "4")))                                        # June, July, August

Personal_Occurence_DF_Descriptive$Season <- factor(Personal_Occurence_DF_Descriptive$Season, levels = c("1", "2", "3", "4"))


