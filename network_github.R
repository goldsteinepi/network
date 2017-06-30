#################
# NICU network analysis
# Citation: Goldstein ND, Eppes SC, Mackley A, Tuttle D, Paul DA. A network model of hand hygiene: How good is good enough to stop the spread of MRSA? Infect Control Hosp Epidemiol. 2017 Jun 28:1-8.
# 12/9/15 -- Neal Goldstein
#################


### FUNCTIONS ###

library("EpiModel") #network modeling
library(gmodels) #CrossTable
library(psych) #describe, describeBy


### PART 1: POWERINSIGHT EXPORT ###

#read data
load("NICU.2016-07-07.RData")
MRNlist = read.csv("NICU.2016-07-07.Identifiers.csv", stringsAsFactors=F)

#subset to 1/1/2015 - 5/31/2015
NICU = NICU[(NICU$Date_admission >= as.Date("2015-01-01")) & (NICU$Date_admission <= as.Date("2015-05-31")), ]

#add missing discharge dates
NICU$Date_discharge_initial[NICU$ID==1868] = as.Date("2015-01-09")
NICU$Date_discharge_initial[NICU$ID==1877] = as.Date("2015-01-10")
NICU$Date_discharge_initial[NICU$ID==1879] = as.Date("2015-01-12")
NICU$Date_discharge_initial[NICU$ID==1893] = as.Date("2015-01-15")
NICU$Date_discharge_initial[NICU$ID==1897] = as.Date("2015-01-15")
NICU$Date_discharge_initial[NICU$ID==1898] = as.Date("2015-01-16")
NICU$Date_discharge_initial[NICU$ID==1899] = as.Date("2015-01-17")
NICU$Date_discharge_initial[NICU$ID==1906] = as.Date("2015-01-17")
NICU$Date_discharge_initial[NICU$ID==1910] = as.Date("2015-01-19")
NICU$Date_discharge_initial[NICU$ID==1914] = as.Date("2015-01-19")
NICU$Date_discharge_initial[NICU$ID==1919] = as.Date("2015-01-21")
NICU$Date_discharge_initial[NICU$ID==1922] = as.Date("2015-01-19")
NICU$Date_discharge_initial[NICU$ID==1930] = as.Date("2015-01-23")
NICU$Date_discharge_initial[NICU$ID==1941] = as.Date("2015-01-29")
NICU$Date_discharge_initial[NICU$ID==1960] = as.Date("2015-02-05")
NICU$Date_discharge_initial[NICU$ID==1971] = as.Date("2015-02-07")
NICU$Date_discharge_initial[NICU$ID==1976] = as.Date("2015-02-07")
NICU$Date_discharge_initial[NICU$ID==1987] = as.Date("2015-02-12")
NICU$Date_discharge_initial[NICU$ID==1991] = as.Date("2015-02-14")
NICU$Date_discharge_initial[NICU$ID==2000] = as.Date("2015-02-19")
NICU$Date_discharge_initial[NICU$ID==2006] = as.Date("2015-02-19")
NICU$Date_discharge_initial[NICU$ID==2014] = as.Date("2015-03-20")
NICU$Date_discharge_initial[NICU$ID==2018] = as.Date("2015-02-22")
NICU$Date_discharge_initial[NICU$ID==2024] = as.Date("2015-02-26")
NICU$Date_discharge_initial[NICU$ID==2026] = as.Date("2015-02-27")
NICU$Date_discharge_initial[NICU$ID==2029] = as.Date("2015-02-26")
NICU$Date_discharge_initial[NICU$ID==2033] = as.Date("2015-02-28")
NICU$Date_discharge_initial[NICU$ID==2037] = as.Date("2015-03-02")
NICU$Date_discharge_initial[NICU$ID==2038] = as.Date("2015-03-01")
NICU$Date_discharge_initial[NICU$ID==2048] = as.Date("2015-03-03")
NICU$Date_discharge_initial[NICU$ID==2052] = as.Date("2015-03-04")
NICU$Date_discharge_initial[NICU$ID==2056] = as.Date("2015-03-06")
NICU$Date_discharge_initial[NICU$ID==2064] = as.Date("2015-03-08")
NICU$Date_discharge_initial[NICU$ID==2066] = as.Date("2015-03-08")
NICU$Date_discharge_initial[NICU$ID==2072] = as.Date("2015-03-08")
NICU$Date_discharge_initial[NICU$ID==2078] = as.Date("2015-03-12")
NICU$Date_discharge_initial[NICU$ID==2086] = as.Date("2015-03-14")
NICU$Date_discharge_initial[NICU$ID==2087] = as.Date("2015-03-15")
NICU$Date_discharge_initial[NICU$ID==2091] = as.Date("2015-03-19")
NICU$Date_discharge_initial[NICU$ID==2092] = as.Date("2015-03-21")
NICU$Date_discharge_initial[NICU$ID==2095] = as.Date("2015-03-23")
NICU$Date_discharge_initial[NICU$ID==2098] = as.Date("2015-03-23")
NICU$Date_discharge_initial[NICU$ID==2108] = as.Date("2015-03-27")
NICU$Date_discharge_initial[NICU$ID==2112] = as.Date("2015-03-29")
NICU$Date_discharge_initial[NICU$ID==2115] = as.Date("2015-03-29")
NICU$Date_discharge_initial[NICU$ID==2120] = as.Date("2015-04-02")
NICU$Date_discharge_initial[NICU$ID==2121] = as.Date("2015-04-03")
NICU$Date_discharge_initial[NICU$ID==2143] = as.Date("2015-04-09")
NICU$Date_discharge_initial[NICU$ID==2145] = as.Date("2015-04-11")
NICU$Date_discharge_initial[NICU$ID==2160] = as.Date("2015-04-17")
NICU$Date_discharge_initial[NICU$ID==2161] = as.Date("2015-04-17")
NICU$Date_discharge_initial[NICU$ID==2163] = as.Date("2015-04-17")
NICU$Date_discharge_initial[NICU$ID==2169] = as.Date("2015-05-01")
NICU$Date_discharge_initial[NICU$ID==2173] = as.Date("2015-04-20")
NICU$Date_discharge_initial[NICU$ID==2187] = as.Date("2015-04-24")
NICU$Date_discharge_initial[NICU$ID==2188] = as.Date("2015-04-27")
NICU$Date_discharge_initial[NICU$ID==2191] = as.Date("2015-04-27")
NICU$Date_discharge_initial[NICU$ID==2195] = as.Date("2015-04-27")
NICU$Date_discharge_initial[NICU$ID==2201] = as.Date("2015-05-01")
NICU$Date_discharge_initial[NICU$ID==2202] = as.Date("2015-04-30")
NICU$Date_discharge_initial[NICU$ID==2206] = as.Date("2015-04-29")
NICU$Date_discharge_initial[NICU$ID==2208] = as.Date("2015-05-01")
NICU$Date_discharge_initial[NICU$ID==2209] = as.Date("2015-05-04")
NICU$Date_discharge_initial[NICU$ID==2212] = as.Date("2015-05-01")
NICU$Date_discharge_initial[NICU$ID==2215] = as.Date("2015-05-02")
NICU$Date_discharge_initial[NICU$ID==2217] = as.Date("2015-05-04")
NICU$Date_discharge_initial[NICU$ID==2221] = as.Date("2015-05-05")
NICU$Date_discharge_initial[NICU$ID==2224] = as.Date("2015-05-07")
NICU$Date_discharge_initial[NICU$ID==2225] = as.Date("2015-05-06")
NICU$Date_discharge_initial[NICU$ID==2227] = as.Date("2015-05-08")
NICU$Date_discharge_initial[NICU$ID==2230] = as.Date("2015-05-09")
NICU$Date_discharge_initial[NICU$ID==2232] = as.Date("2015-05-07")
NICU$Date_discharge_initial[NICU$ID==2240] = as.Date("2015-05-11")
NICU$Date_discharge_initial[NICU$ID==2244] = as.Date("2015-05-12")
NICU$Date_discharge_initial[NICU$ID==2245] = as.Date("2015-05-12")
NICU$Date_discharge_initial[NICU$ID==2247] = as.Date("2015-05-13")
NICU$Date_discharge_initial[NICU$ID==2249] = as.Date("2015-05-14")
NICU$Date_discharge_initial[NICU$ID==2253] = as.Date("2015-05-15")
NICU$Date_discharge_initial[NICU$ID==2262] = as.Date("2015-05-16")
NICU$Date_discharge_initial[NICU$ID==2267] = as.Date("2015-05-24")
NICU$Date_discharge_initial[NICU$ID==2276] = as.Date("2015-06-02")
NICU$Date_discharge_initial[NICU$ID==2281] = as.Date("2015-05-22")
NICU$Date_discharge_initial[NICU$ID==2283] = as.Date("2015-05-22")
NICU$Date_discharge_initial[NICU$ID==2287] = as.Date("2015-06-30")
NICU$Date_discharge_initial[NICU$ID==2288] = as.Date("2015-05-22")
NICU$Date_discharge_initial[NICU$ID==2293] = as.Date("2015-06-01")
NICU$Date_discharge_initial[NICU$ID==2300] = as.Date("2015-05-30")
NICU$Date_discharge_initial[NICU$ID==2316] = as.Date("2015-06-02")
NICU$Date_discharge_initial[NICU$ID==2320] = as.Date("2015-06-01")
NICU$Date_discharge_initial[NICU$ID==2331] = as.Date("2015-06-03")
NICU$Date_discharge_initial[NICU$ID==2334] = as.Date("2015-06-03")
NICU$Date_discharge_initial[NICU$ID==2336] = as.Date("2015-06-03")

#get mrns
mrns = NA
for (i in 1:nrow(NICU))
{
  mrns = c(mrns,MRNlist$MRN[which(MRNlist$ID==NICU$ID[i])])
}
mrns = mrns[-1]

#write the mrns to clipboard for paste into PowerInsight
writeClipboard(paste(mrns, collapse=";"))

## run PowerInsight query: Documentation Forms
## when saving, select Network, Client, then the appropriate drive, etc
## rename CSV columns as follows: Description, DateTime, Personnel, Role, MRN

## run PowerInsight query: Location History
## when saving, select Network, Client, then the appropriate drive, etc
## rename CSV columns as follows: MRN, Location, Unit, DateTime

## run SafetySurveillor query: Respiratory Cultures in SCN for 1/1-5/31/15
## rename CSV columns as follows: DateTime, Name, MRN, Age, Gender, Source, Organism, Ward, Service

#read PowerInsight & SafetySurveillor exports
pt_interactions = read.csv("Documentation Forms query.csv", stringsAsFactors=F)
pt_locations = read.csv("Location History query.csv", stringsAsFactors=F)
pt_cultures = read.csv("Safety Surveillor Jan1-May31 2015.csv", stringsAsFactors=F)

#set date variable type
pt_interactions$Date = as.Date(pt_interactions$DateTime, format="%m/%d/%Y")
pt_interactions$Hour = as.numeric(format.Date(strptime(pt_interactions$DateTime, format="%m/%d/%Y %H:%M"), "%H"))
pt_interactions$Minute = as.numeric(format.Date(strptime(pt_interactions$DateTime, format="%m/%d/%Y %H:%M"), "%M"))
pt_locations$DateTime = as.POSIXlt(pt_locations$DateTime, format="%m/%d/%Y %H:%M")
pt_cultures$DateTime = as.Date(pt_cultures$DateTime, format="%m/%d/%Y")

#sort by MRN and date
pt_interactions = pt_interactions[order(pt_interactions$MRN, pt_interactions$Date), ]
pt_locations = pt_locations[order(pt_locations$MRN, pt_locations$DateTime), ]
pt_cultures = pt_cultures[order(pt_cultures$MRN, pt_cultures$DateTime), ]

#revert mrns to ids: interactions
mrns = as.numeric(unique(pt_interactions$MRN))
for (i in 1:length(mrns))
{
  #ID for this MRN
  mrn_id = MRNlist$ID[which(MRNlist$MRN==mrns[i])]
  
  #only set when there is an exact MRN/ID match
  if (length(mrn_id)==1)
  {
    pt_interactions$MRN[which(pt_interactions$MRN==mrns[i])] = MRNlist$ID[which(MRNlist$MRN==mrns[i])]
  } else {
    pt_interactions$MRN[which(pt_interactions$MRN==mrns[i])] = NA
  } 
}

pt_interactions$ID = pt_interactions$MRN
pt_interactions$MRN = NULL
row.names(pt_interactions) = NULL

#revert mrns to ids: location
mrns = as.numeric(unique(pt_locations$MRN))
for (i in 1:length(mrns))
{
  #ID for this MRN
  mrn_id = MRNlist$ID[which(MRNlist$MRN==mrns[i])]
  
  #only set when there is an exact MRN/ID match
  if (length(mrn_id)==1)
  {
    pt_locations$MRN[which(pt_locations$MRN==mrns[i])] = MRNlist$ID[which(MRNlist$MRN==mrns[i])]
  } else {
    pt_locations$MRN[which(pt_locations$MRN==mrns[i])] = NA
  } 
}

pt_locations$ID = pt_locations$MRN
pt_locations$MRN = NULL
row.names(pt_locations) = NULL

#revert mrns to ids: cultures
mrns = as.numeric(unique(pt_cultures$MRN))
for (i in 1:length(mrns))
{
  #ID for this MRN
  mrn_id = MRNlist$ID[which(MRNlist$MRN==mrns[i])]
  
  #only set when there is an exact MRN/ID match
  if (length(mrn_id)==1)
  {
    pt_cultures$MRN[which(pt_cultures$MRN==mrns[i])] = MRNlist$ID[which(MRNlist$MRN==mrns[i])]
  } else {
    pt_cultures$MRN[which(pt_cultures$MRN==mrns[i])] = NA
  } 
}

pt_cultures$ID = pt_cultures$MRN
pt_cultures$MRN = NULL
row.names(pt_cultures) = NULL

#cleanup
writeClipboard("")
rm(MRNlist,mrns,i,mrn_id)

#remove interactions that did not occur during the NICU stay
droplist = NA
for (i in 1:length(unique(pt_interactions$ID)))
{
  cat("\n\n************** ","Observation: ",i," **************\n",sep="")
  
  #get current ID
  currentID = unique(pt_interactions$ID)[i]
  
  #interactions that have occurred before or after discharge
  events = which((pt_interactions$ID==currentID) & ((pt_interactions$Date < NICU$Date_admission[NICU$ID==currentID]) | (pt_interactions$Date > NICU$Date_discharge_initial[NICU$ID==currentID])))

  #record interactions to drop
  if (length(events)>0)
    droplist = c(droplist, -1*events)
}
pt_interactions = pt_interactions[droplist[-1], ]
rm(i,currentID,events,droplist)

#drop A/B/C from bed and set as numeric
pt_locations$Location = as.numeric(gsub("[A-Z]","",pt_locations$Location))

#capture length of time in each of the main areas
NICU$Location_200 = NA
NICU$Location_201_209 = NA
NICU$Location_210_217 = NA
NICU$Location_218_223 = NA
NICU$Location_224_225 = NA
NICU$Location_longest = NA
for (i in 1:length(unique(pt_locations$ID)))
{
  cat("\n\n************** ","Observation: ",i," **************\n",sep="")

  #get current ID
  currentID = unique(pt_locations$ID)[i]
  
  #all locations for this infant
  locations = pt_locations[pt_locations$ID==currentID, ]
  
  #add discharge info
  locations = rbind(locations,data.frame(Location=locations$Location[nrow(locations)], Unit="CSCN", DateTime=as.POSIXlt(paste(NICU$Date_discharge_initial[NICU$ID==locations$ID[nrow(locations)]],"23:59"), format="%Y-%m-%d %H:%M"), ID=locations$ID[nrow(locations)]))
  
  #track which bed the infant was in for the longest point in the stay in hours
  #longest_bed = locations$Location[1]
  #bed_time = as.numeric(difftime(locations$DateTime[2],locations$DateTime[1], units="hours"))
  
  #track time by location in hours
  Location_200 = 0
  Location_201_209 = 0
  Location_210_217 = 0
  Location_218_223 = 0
  Location_224_225 = 0
  Location_unknown = 0
  
  #go through each location to determine time in each area
  while(nrow(locations)>1)
  {
    if (is.na(locations$Location[1]))
      locations$Location[1]=999
    
    if (locations$Location[1]==200) {
      Location_200 = Location_200 + as.numeric(difftime(locations$DateTime[2],locations$DateTime[1], units="hours"))
    } else if (locations$Location[1]>=201 && locations$Location[1]<=209) {
      Location_201_209 = Location_201_209 + as.numeric(difftime(locations$DateTime[2],locations$DateTime[1], units="hours"))
    } else if (locations$Location[1]>=210 && locations$Location[1]<=217) {
      Location_210_217 = Location_210_217 + as.numeric(difftime(locations$DateTime[2],locations$DateTime[1], units="hours"))
    } else if (locations$Location[1]>=218 && locations$Location[1]<=223) {
      Location_218_223 = Location_218_223 + as.numeric(difftime(locations$DateTime[2],locations$DateTime[1], units="hours"))
    } else if (locations$Location[1]>=224 && locations$Location[1]<=225) {
      Location_224_225 = Location_224_225 + as.numeric(difftime(locations$DateTime[2],locations$DateTime[1], units="hours"))
    } else {
      Location_unknown = Location_unknown + as.numeric(difftime(locations$DateTime[2],locations$DateTime[1], units="hours"))
    }

    locations = locations[-1, ]
  }
  
  #longest location
  nhrs = max(Location_200, Location_201_209, Location_210_217, Location_218_223, Location_224_225)

  #copy to NICU dataset
  NICU$Location_200[which(NICU$ID==currentID)] = Location_200
  NICU$Location_201_209[which(NICU$ID==currentID)] = Location_201_209
  NICU$Location_210_217[which(NICU$ID==currentID)] = Location_210_217
  NICU$Location_218_223[which(NICU$ID==currentID)] = Location_218_223
  NICU$Location_224_225[which(NICU$ID==currentID)] = Location_224_225
  NICU$Location_longest[which(NICU$ID==currentID)] = ifelse(Location_200==nhrs, "Location_200", ifelse(Location_201_209==nhrs, "Location_201_209", ifelse(Location_210_217==nhrs, "Location_210_217", ifelse(Location_218_223==nhrs, "Location_218_223", ifelse(Location_224_225==nhrs, "Location_224_225", NA)))))
  
}
rm(i,currentID,pt_locations,locations,nhrs,Location_200,Location_201_209,Location_210_217,Location_218_223,Location_224_225,Location_unknown)

#add culture results to NICU dataset
NICU$Vent_culture = NA
NICU$Vent_culture_positive = NA
NICU$Vent_culture_organisms = NA
for (i in 1:nrow(NICU))
{
  cat("\n\n************** ","Observation: ",i," **************\n",sep="")
  
  #check if intubated
  if (NICU$Vent[i]==1)
  {
    #check if culture results are available
    if (length(pt_cultures$Organism[pt_cultures$ID==NICU$ID[i]])>0) {
      
      #positive culture, save organisms
      NICU$Vent_culture[i] = 1
      NICU$Vent_culture_positive[i] = 1
      NICU$Vent_culture_organisms[i] = paste(unique(pt_cultures$Organism[pt_cultures$ID==NICU$ID[i]]), collapse=";")
      
    } else {
      
      #check if intubated on a Monday (when respiratory cultures are taken)
      #fot future, add Vent_state_date to dataset
      if (weekdays(NICU$Date_admission[i]+NICU$Vent_start[i])=="Sunday" && NICU$Vent_length[i]>=1) {
        NICU$Vent_culture[i] = 1
        NICU$Vent_culture_positive[i] = 0
      } else if (weekdays(NICU$Date_admission[i]+NICU$Vent_start[i])=="Monday" && NICU$Vent_length[i]>=7) {
        NICU$Vent_culture[i] = 1
        NICU$Vent_culture_positive[i] = 0
      } else if (weekdays(NICU$Date_admission[i]+NICU$Vent_start[i])=="Tuesday" && NICU$Vent_length[i]>=6) {
        NICU$Vent_culture[i] = 1
        NICU$Vent_culture_positive[i] = 0
      } else if (weekdays(NICU$Date_admission[i]+NICU$Vent_start[i])=="Wednesday" && NICU$Vent_length[i]>=5) {
        NICU$Vent_culture[i] = 1
        NICU$Vent_culture_positive[i] = 0
      } else if (weekdays(NICU$Date_admission[i]+NICU$Vent_start[i])=="Thursday" && NICU$Vent_length[i]>=4) {
        NICU$Vent_culture[i] = 1
        NICU$Vent_culture_positive[i] = 0
      } else if (weekdays(NICU$Date_admission[i]+NICU$Vent_start[i])=="Friday" && NICU$Vent_length[i]>=3) {
        NICU$Vent_culture[i] = 1
        NICU$Vent_culture_positive[i] = 0
      } else if (weekdays(NICU$Date_admission[i]+NICU$Vent_start[i])=="Saturday" && NICU$Vent_length[i]>=2) {
        NICU$Vent_culture[i] = 1
        NICU$Vent_culture_positive[i] = 0
      } else {
        NICU$Vent_culture[i] = 0
      }
        
    }
  }
}
rm(i,pt_cultures)

rownames(NICU) = NULL
rownames(pt_interactions) = NULL

#save
save.image(file="network.RData")


### PART 2: CLEAN and RECODE ###

#read data
load("network.RData")

#remove non-relevant pt interactions
pt_interactions = pt_interactions[!(pt_interactions$Description=="NICU Nutrition" |
                                    pt_interactions$Description=="Initial Discharge Planning Assessment" |
                                    pt_interactions$Description=="High Risk Family Communication" |
                                    pt_interactions$Description=="Ongoing Discharge Planning Assessment" |
                                    pt_interactions$Description=="Nutritional Assessment" |
                                    pt_interactions$Description=="Care Management Team Documentation" |
                                    pt_interactions$Description=="High Risk Family Communication-NICU" |
                                    pt_interactions$Description=="Milk Bank" |
                                    pt_interactions$Description=="NICU  Follow Up Call" |
                                    pt_interactions$Description=="Audiology IHAP" |
                                    pt_interactions$Description=="C4A Nursing Charges" |
                                    pt_interactions$Description=="ED Nursing Home Medications" |
                                    pt_interactions$Description=="Perinatal Discharge Summary - Newborn" |
                                    pt_interactions$Description=="Finance Charges" |
                                    pt_interactions$Description=="Appointment After Discharge GI -NICU" |
                                    pt_interactions$Description=="Appointment After Discharge Eye -NICU" |
                                    pt_interactions$Description=="Allergy" |
                                    pt_interactions$Description=="Vaccine Assessment - Influenza <13 Yrs" |
                                    pt_interactions$Description=="Critical Test Result Information" |
                                    pt_interactions$Description=="Critical Test Result Communication Form" |
                                    pt_interactions$Description=="Appointment(s) for Patient After Discharge - NICU" |
                                    pt_interactions$Description=="OB-GYN Allergies/Med History" |
                                    pt_interactions$Description=="Universal Protocol" |
                                    pt_interactions$Description=="Appointment After Discharge Pulm -NICU" |
                                    pt_interactions$Description=="Appointment After Discharge Card -NICU" |
                                    pt_interactions$Description=="ED Tech Depart" |
                                    pt_interactions$Description=="Home Wound Care" |
                                    pt_interactions$Description=="Appointment After Discharge Neuro -NICU" |
                                    pt_interactions$Description=="Constipation Assessment/Treatment" |
                                    pt_interactions$Description=="Medication History" |
                                    pt_interactions$Description=="Rehab Discharge Instructions" |
                                    pt_interactions$Description=="Nursing Profile" |
                                    pt_interactions$Description=="Nursing Newborn Profile" |
                                    pt_interactions$Description=="Pediatric Growth" |
                                    pt_interactions$Description=="NICU Provider Summary" |
                                    pt_interactions$Description=="VAP Prevention PI Tool"), ]

#keep highest risk pt interactions
# pt_interactions = pt_interactions[pt_interactions$Description=="Blood Product Transfusion Volumes" |
#                                     pt_interactions$Description=="Central Line Insertion Checklist" |
#                                     pt_interactions$Description=="Foley Catheter Indication Assessment" |
#                                     pt_interactions$Description=="SCN Proc Chest Tube" |
#                                     pt_interactions$Description=="SCN Proc ET Intubation" |
#                                     pt_interactions$Description=="SCN Proc Lumbar Puncture" |
#                                     pt_interactions$Description=="SCN Proc Needle Thoracostomy" |
#                                     pt_interactions$Description=="SCN Proc PAL" |
#                                     pt_interactions$Description=="SCN Proc PICC" |
#                                     pt_interactions$Description=="SCN Proc Umbilical Artery Cath" |
#                                     pt_interactions$Description=="SCN Proc Umbilical Vein Cath" |
#                                     pt_interactions$Description=="Site/Dressing Assess PICC/Port" |
#                                     pt_interactions$Description=="Urinary Catheter Insertion Checklist" |
#                                     pt_interactions$Description=="VBG Draw By", ]

#simplify roles
pt_interactions$Role = ifelse(pt_interactions$Role=="RN NICU", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Respiratory Staff", "Therapists (Resp, Speech, OT/PT)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="PP RN II", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Nurse Practitioner NICU", "Residents, Attending, NNPs, Students (Med/NNP)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Staff Nurse", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="RN II", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Occupational Therapist", "Therapists (Resp, Speech, OT/PT)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Resp Student", "Therapists (Resp, Speech, OT/PT)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Physical Therapist", "Therapists (Resp, Speech, OT/PT)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="LD RN", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Doctor PEDS", "Residents, Attending, NNPs, Students (Med/NNP)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="PP RN", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="LPN CLBR", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Unit Clerk WC", "Other/Unknown", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="RN", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Resident", "Residents, Attending, NNPs, Students (Med/NNP)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Speech Therapist", "Therapists (Resp, Speech, OT/PT)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="EEG Tech", "Ancillary Techs (Medical Imaging)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="PCT II", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="LPN", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Nurse Practitioner", "Residents, Attending, NNPs, Students (Med/NNP)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="MRI Staff", "Ancillary Techs (Medical Imaging)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Doctor NICU", "Residents, Attending, NNPs, Students (Med/NNP)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="EKG Tech", "Ancillary Techs (Medical Imaging)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="IPASS Resident", "Residents, Attending, NNPs, Students (Med/NNP)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="PCT", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="RN Oncology", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Unit Clerk WC II", "Other/Unknown", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="ED ECT", "Ancillary Techs (Medical Imaging)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="ED Nurse", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="WOC Nurse", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Rad Staff", "Ancillary Techs (Medical Imaging)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Audiologist", "Therapists (Resp, Speech, OT/PT)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Bed Control", "Other/Unknown", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="LD RN II", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="PeriOp Nurse", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Clinical Staff Documentation", "Other/Unknown", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="ED Residents", "Residents, Attending, NNPs, Students (Med/NNP)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="PCT II WC", "Nurse (RN, LPN, Tech)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="DBA LockedSearch", "Other/Unknown", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Doctor", "Residents, Attending, NNPs, Students (Med/NNP)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="HIMS Chart Print", "Other/Unknown", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role=="Nurse Practitioner Student", "Residents, Attending, NNPs, Students (Med/NNP)", pt_interactions$Role)
pt_interactions$Role = ifelse(pt_interactions$Role==" ", "Other/Unknown", pt_interactions$Role)

# #add indicators of care
# pt_interactions$Gestation = NA
# pt_interactions$Intubation = NA
# pt_interactions$Cooling = NA
# 
# for (i in 1:length(unique(pt_interactions$ID)))
# {
#   cat("\n\n************** ","Observation: ",i," **************\n",sep="")
#   
#   #get current ID
#   currentID = unique(pt_interactions$ID)[i]
#   
#   #copy variables
#   pt_interactions$Gestation[pt_interactions$ID==currentID] = NICU$Gestational_age[NICU$ID==currentID]
#   pt_interactions$Intubation[pt_interactions$ID==currentID] = NICU$Vent[NICU$ID==currentID]
#   pt_interactions$Cooling[pt_interactions$ID==currentID] = ifelse(NICU$Cooling[NICU$ID==currentID]>0, 1, 0)
# }
# rm(i,currentID)

#preterm
#pt_interactions$Preterm = ifelse(pt_interactions$Gestation<37, 1, 0)
NICU$Preterm = ifelse(NICU$Gestation<37, 1, 0)

#lbw
NICU$LBW = ifelse(NICU$Birthweight<2500, 1, 0)


### PART 3: CALCULATE DEGREE DISTRIBUTION ###

#census for denominator per day
census = NA

#tracks the overall number of interactions per hour per infant
total_interactions = NA

#tracks total edges on per infant
NICU$Network_edges_total = 0

#tracks the overall number of colonizations per day
total_ETcolonizations = NA
total_MRSAcolonizations = NA
NICU$Vent_start_date = NICU$Date_admission+NICU$Vent_start
NICU$Vent_end_date = NICU$Date_admission+NICU$Vent_start+NICU$Vent_length

#avg number of admissions and discharges per day
total_admit = NA
total_discharge = NA

#go through each day in the NICU and count degree distribution per hour per infant
for (i in 1:as.numeric(as.Date("2015-05-31")-as.Date("2015-01-01")))
{
  cat("\n\n************** ","Observation: ",i," **************\n",sep="")
  
  today = as.Date("2015-01-01")+i-1

  #get census as daily total since we don't have hourly
  todays_census = NICU$Census_admission[NICU$Date_admission==today][1]
  
  #impute missing census (no admissions that day) by averaging two days before and after
  if (is.na(todays_census))
    todays_census = round(mean(c(NICU$Census_admission[NICU$Date_admission==today-2][1], NICU$Census_admission[NICU$Date_admission==today-1][1], NICU$Census_admission[NICU$Date_admission==today+1][1], NICU$Census_admission[NICU$Date_admission==today+2][1]), na.rm=T))

  census = c(census, todays_census)

  #go through each hour
  for (j in 0:23)
  {
    hourly_interactions = na.omit(pt_interactions[pt_interactions$Date==today & pt_interactions$Hour==j, c("Personnel","Role","ID")])
    
    #found possible edges
    if (nrow(hourly_interactions)>1)
    {
      #get list of infants who were involved in an interaction this hour
      hourly_infants = unique(hourly_interactions$ID)
      
      #for each infant
      for (k in 1:length(hourly_infants))
      {
        #provider list for this infant
        provider_list = unique(hourly_interactions$Personnel[which(hourly_interactions$ID==hourly_infants[k])])
        
        #check if these providers have seen any other infants this hour (will include current infant), subtract one for the current infant
        provider_interactions = length(unique(hourly_interactions$ID[which(hourly_interactions$Personnel %in% provider_list)])) - 1
        
        #track total number of provider interactions
        total_interactions = c(total_interactions, nrow(hourly_interactions[hourly_interactions$ID==hourly_infants[k],]))
        
        #track total edges for this infant for mean degree by infant calculation
        NICU$Network_edges_total[NICU$ID==hourly_infants[k]] = NICU$Network_edges_total[NICU$ID==hourly_infants[k]] + provider_interactions
      } 
      
      #add infants that did not receive any care count for this hour
      total_interactions = c(total_interactions, rep(0,todays_census-length(hourly_infants)))
      
    } else {
      
      #add infants that did not receive any care count for this hour
      total_interactions = c(total_interactions, rep(0,todays_census))
      
    }
  }
  
  #track total ETT colonizations per day; since not all intubations have received a culture, need to artificially inflate by 50% (based on prediction of those with a culture, how often is it positive)
  #CrossTable(NICU$Vent_culture_positive, NICU$Vent_culture)
  total_ETcolonizations = c(total_ETcolonizations, sum(NICU$Vent_culture_positive[NICU$Vent_start_date<=today & NICU$Vent_end_date>=today], na.rm=T) + 0.5*sum(NICU$Vent[NICU$Vent_culture==0 & NICU$Vent_start_date<=today & NICU$Vent_end_date>=today], na.rm=T))
  
  #track total MRSA colonizations per day
  total_MRSAcolonizations = c(total_MRSAcolonizations, sum(NICU$MRSA_colonization[NICU$Date_admission<=today & NICU$Date_discharge_initial>=today], na.rm=T))
  
  #track total admit and discharges
  total_admit = c(total_admit,sum(NICU$Date_admission==today))
  total_discharge = c(total_discharge,sum(NICU$Date_discharge_initial==today))
  
}
rm(i,j,k,hourly_infants,provider_interactions,provider_list,today,todays_census,hourly_interactions)

#average number of edges per infant per hour
NICU$Network_edges_avg = NA

for (i in 1:nrow(NICU))
{
  cat("\n\n************** ","Observation: ",i," **************\n",sep="")
  
  #compute hours at risk for contact
  admitDate = ifelse(NICU$Date_admission[i]<as.Date("2015-01-01"), as.Date("2015-01-01"), NICU$Date_admission[i])
  dischargeDate = ifelse(NICU$Date_discharge_initial[i]>as.Date("2015-05-31"), as.Date("2015-05-31"), NICU$Date_discharge_initial[i])
  NICU$Network_edges_avg[i] = round(NICU$Network_edges_total[i] / ((dischargeDate-admitDate+1)*24), 0)
}
rm(i,admitDate,dischargeDate)


### PART 4: CALCULATE NETWORK STATS ###

#average number of edges per infant per hour in the NICU overall and by network attributes
mean_deg_overall = round(mean(NICU$Network_edges_avg, na.rm=T),1)
mean_deg_preterm = round(mean(NICU$Network_edges_avg[NICU$Preterm==1], na.rm=T),1)
mean_deg_vent = round(mean(NICU$Network_edges_avg[NICU$Vent==1], na.rm=T),1)
mean_deg_fronthall = round(mean(NICU$Network_edges_avg[NICU$Location_longest=="Location_201_209"], na.rm=T),1)
mean_deg_backhall = round(mean(NICU$Network_edges_avg[NICU$Location_longest=="Location_210_217"], na.rm=T),1)
mean_deg_quietrm = round(mean(NICU$Network_edges_avg[NICU$Location_longest=="Location_218_223"], na.rm=T),1)

#distribution of edges per infant per hour in the NICU overall
table(NICU$Network_edges_avg, useNA="always")

#average network size (number of infants in the NICU on a given day)
network_size = round(mean(census, na.rm=T))

#number of hours to model based on the median length of stay in the NICU
network_steps = median(NICU$LOS, na.rm=T)*24

#duration of at-risk can correspond to the average shift, in hours
network_duration = 12

#number of triangles on average for an hour
triangles = 2

#ratio of LOS by outcome
LOS_ETcolonized = mean(NICU$LOS[NICU$Vent_culture==1 & NICU$Vent_culture_positive==1],na.rm=T) / mean(NICU$LOS[NICU$Vent_culture==1 & NICU$Vent_culture_positive==0],na.rm=T)
LOS_MRSAcolonized = 1.922454 #computed from 2010-2015 NICU dataset: mean(NICU$LOS[NICU$MRSA_colonization==1 & NICU$Gestational_age<37 & NICU$Vent==0],na.rm=T) / mean(NICU$LOS[NICU$MRSA_colonization==0 & NICU$Gestational_age<37 & NICU$Vent==0],na.rm=T)

#mean degree distribution for the average network size
deg_distr = prop.table(table(NICU$Network_edges_avg))*network_size

#expected degree distribution
dpois(0:3, mean_deg_overall)*network_size

#degree distribution by network characteristics
describeBy(NICU$Network_edges_avg, NICU$Preterm)
describeBy(NICU$Network_edges_avg, NICU$Vent)

#what are the primary drivers of contact?
summary(lm(Network_edges_avg ~ Preterm + LBW + LOS + Cooling + Vent, data=NICU))


### PART 5: MODEL NETWORKS ###

#create a network of infants in the NICU
NICU_network = network.initialize(n=network_size, directed=F)
plot(NICU_network)

#edges: avg number of edges in the NICU for a given hour (multiply each degree by nodes with that degree), divided by two since the statistic is for number of edges (not number of nodes)
target_stats1_edges = round(sum(deg_distr*0:3)/2, 1)

#concurrent: avg number of infants with degree>=2 in the NICU for a given hour
target_stats1_concurrent = round(sum(deg_distr[3:4]), 1)

#degree(0): avg number of infants with degree=0 in the NICU for a given hour
target_stats1_deg0 = round(sum(deg_distr[1]), 1)

#degree(1): avg number of infants with degree=1 in the NICU for a given hour
target_stats1_deg1 = round(sum(deg_distr[2]), 1)

#degree(2): avg number of infants with degree=2 in the NICU for a given hour
target_stats1_deg2 = round(sum(deg_distr[3]), 1)

#degree(3): avg number of infants with degree=3 in the NICU for a given hour
target_stats1_deg3 = round(sum(deg_distr[4]), 1)

#avg number of edges by location for a given hour, even distribution among the three primary locations
NICU_network = set.vertex.attribute(NICU_network, attrname="location", value=c(rep("fronthall",18), rep("backhall",17), rep("quietrm",17)))
target_stats1_fronthall = as.numeric(round(mean_deg_fronthall*18, 1))
target_stats1_backhall = as.numeric(round(mean_deg_backhall*17, 1))
target_stats1_quietrm = as.numeric(round(mean_deg_quietrm*17, 1))

#average edges per location, assume 90% of care happens within location
target_stats1_location = round(target_stats1_edges*0.9)

#avg number of edges for a preterm infant for a given hour, computed by multiplying the mean degree of preterm by # of preterm infants
#set the intial values to ensure an even distribution by the 3 locations
#total distribution: CrossTable(NICU$Preterm)
NICU_network = set.vertex.attribute(NICU_network, attrname="preterm", value=c(rep("no",10),rep("yes",8),rep("no",9),rep("yes",8),rep("no",9),rep("yes",8)))
target_stats1_preterm = as.numeric(round(mean_deg_preterm*prop.table(table(NICU$Preterm))[2]*network_size, 1))

#avg number of edges for a ventilated infant for a given hour, computed by multiplying the mean degree of vent by # of vented infants
#set the intial values to ensure an even distribution by the 3 locations and by preterm
#total distribution: CrossTable(NICU$Preterm, NICU$Vent)
NICU_network = set.vertex.attribute(NICU_network, attrname="vent", value=c(rep("no",9),rep("yes",1),rep("no",6),rep("yes",2),rep("no",8),rep("yes",1),rep("no",6),rep("yes",2),rep("no",8),rep("yes",1),rep("no",6),rep("yes",2)))
target_stats1_vent = as.numeric(round(mean_deg_vent*prop.table(table(NICU$Vent))[2]*network_size, 1))

#average duration of edges in hrs
coef_diss1 = dissolution_coefs(dissolution = ~offset(edges), duration=network_duration)

#estimate the dynamic model
model1_dynamic = netest(nw=NICU_network, formation=~edges+concurrent+degree(1)+gwesp(0,fixed=T)+nodefactor("preterm")+nodefactor("vent")+nodematch("location"), target.stats=c(target_stats1_edges,target_stats1_concurrent,target_stats1_deg1,triangles,target_stats1_preterm,target_stats1_vent,target_stats1_location), coef.diss=coef_diss1, edapprox=T)
summary(model1_dynamic)

#simulate dynamic networks from model and diagnostics 
sim1_dynamic = netdx(model1_dynamic, nsteps=network_steps, nsims=20, ncores=4, nwstats.formula=~edges+concurrent+degree(0:3)+gwesp(0,fixed=T)+nodefactor("preterm")+nodefactor("vent")+nodematch("location"))
sim1_dynamic

#output to tif for publication 
#tiff("Figure1_supplement.tif",height=6,width=10,units='in',res=1200) 
plot(sim1_dynamic)
#dev.off() 

#save ergm
save.image("network_ergm.RData")
#load("network_ergm.RData")


### PART 6: MODEL EPIDEMIC ###

#set colonization assumptions and infection parameters
ADT_admit = round(mean(total_admit/census,na.rm=T)/24,4) #average admission per day, computed as rate per person per hour
#ADT_disch = round(mean(total_discharge/census,na.rm=T)/24,4) #average discharge per day, computed as rate per person per hour
ADT_disch = ADT_admit #set as admission rate to balance network size
ADT_colon =  round(mean(total_admit/census,na.rm=T)/24 * 1/LOS_MRSAcolonized,4) #average discharge rate per hr if colonized, computed by ratio of LOS
inf_prob = 0.15*0.05 #probabilty of colonization per provider interaction
#inf_prob = 0.075*0.05 #probabilty of colonization per provider interaction, sensitivity analysis
n_colon = median(total_MRSAcolonizations,na.rm=T) #number of infants initially MRSA colonized
hygiene_eff = 0.48 #effectiveness computed as product of compliance and efficacy from literature
n_sims = 100 #number of simulations

#simulate network attributes
mrsa_sim_vent = netsim(model1_dynamic, param.net(inf.prob=inf_prob, act.rate=mean_deg_overall, b.rate=ADT_admit, ds.rate=ADT_disch, di.rate=ADT_colon, inter.eff=hygiene_eff), init.net(i.num=n_colon, status.rand=F), control.net(type = "SI", nsteps=network_steps, nsims=n_sims, ncores=4, epi.by="vent"))
mrsa_sim_preterm = netsim(model1_dynamic, param.net(inf.prob=inf_prob, act.rate=mean_deg_overall, b.rate=ADT_admit, ds.rate=ADT_disch, di.rate=ADT_colon, inter.eff=hygiene_eff), init.net(i.num=n_colon, status.rand=F), control.net(type = "SI", nsteps=network_steps, nsims=n_sims, ncores=4, epi.by="preterm"))
mrsa_sim_location = netsim(model1_dynamic, param.net(inf.prob=inf_prob, act.rate=mean_deg_overall, b.rate=ADT_admit, ds.rate=ADT_disch, di.rate=ADT_colon, inter.eff=hygiene_eff), init.net(i.num=n_colon, status.rand=F), control.net(type = "SI", nsteps=network_steps, nsims=n_sims, ncores=4, epi.by="location"))
save.image("network_sim_attributes.RData")

#simulate a longer length of stay 
network_steps = max(NICU$LOS,na.rm=T)*24
hygiene_eff = 0.88
n_sims=1
n_tries=0
n_success=0
while(n_success<=100)
{
  cat("\n\n************** ","n_tries: ",n_tries,"; n_success: ",n_success," **************\n",sep="")
  
  tmp = tryCatch(netsim(model1_dynamic, param.net(inf.prob=inf_prob, act.rate=mean_deg_overall, b.rate=ADT_admit, ds.rate=ADT_disch, di.rate=ADT_colon, inter.eff=hygiene_eff), init.net(i.num=n_colon, status.rand=F), control.net(type = "SI", nsteps=network_steps, nsims=n_sims, ncores=4, verbose=F)), error=function(e) NA)
  
  if (is.list(tmp))
  {
    n_success = n_success + 1
    
    if (n_success==1){
      mrsa_sim_los = tmp
    } else {
      mrsa_sim_los = merge(mrsa_sim_los, tmp)
    }
  }
  n_tries = n_tries + 1

  rm(mrsa_sim_los)
  gc()
}
rm(n_tries,n_success,tmp)
save.image("network_sim_los.RData")

#simulate hygiene effectiveness and prevalance of MRSA
n_colon = seq(1,max(total_MRSAcolonizations,na.rm=T),by=1) #number of infants initially MRSA colonized
hygiene_eff = c(0,0.24,0.48,0.68,0.88,1)
results = data.frame("colonized"=NA,"effectivness"=NA,"prevalence"=NA,"denominator"=NA,"prev_low"=NA,"prev_high"=NA,stringsAsFactors=F)
for (i in 1:length(n_colon))
{
  for (j in 1:length(hygiene_eff))
  {
    cat("\n\n************** ","n_colon: ",n_colon[i],"; hygiene_eff: ",hygiene_eff[j]," **************\n",sep="")

    #simulate and capture network parameters
    mrsa_sim = netsim(model1_dynamic, param.net(inf.prob=inf_prob, act.rate=mean_deg_overall, b.rate=ADT_admit, ds.rate=ADT_disch, di.rate=ADT_colon, inter.eff=hygiene_eff[j]), init.net(i.num=n_colon[i], status.rand=F), control.net(type = "SI", nsteps=network_steps, nsims=n_sims, ncores=4, verbose=F))
    mrsa_sim_mean = as.data.frame(mrsa_sim, out="mean")
    mrsa_sim_sd = as.data.frame(mrsa_sim, out="sd")

    #compute CI
    conf_int_low = mrsa_sim_mean$i.num[network_steps] - 1.96*(mrsa_sim_sd$i.num[network_steps]/sqrt(n_sims))
    conf_int_high = mrsa_sim_mean$i.num[network_steps] + 1.96*(mrsa_sim_sd$i.num[network_steps]/sqrt(n_sims))
    
    #store results
    results = rbind(results, data.frame("colonized"=n_colon[i],"effectivness"=hygiene_eff[j],"prevalence"=mrsa_sim_mean$i.num[network_steps],"denominator"=mrsa_sim_mean$num[network_steps],"prev_low"=conf_int_low,"prev_high"=conf_int_high,stringsAsFactors=F))
    
    rm(mrsa_sim,mrsa_sim_mean,mrsa_sim_sd,conf_int_low,conf_int_high)
    gc()
  }
}
rm(i,j)
results = results[-1,]
rownames(results) = NULL

#save results
save.image("network_sim_hygiene_0.75percontact.RData")
#save.image("network_sim_hygiene_0.375percontact.RData")

mrsa_sim_preterm
plot(mrsa_sim_preterm)
plot(mrsa_sim_preterm, sim.lines = TRUE, mean.line = FALSE, qnts = FALSE, popfrac = FALSE)
plot(mrsa_sim_preterm, mean.smooth = FALSE, qnts = 1, qnts.smooth = FALSE, popfrac = FALSE)

#flow size corresponds to the flow between infection and recovery, "si" shows incidence
plot(mrsa_sim_preterm, y = c("si.flow"), qnts = FALSE, leg = TRUE, main = "Flow Sizes")

#plot overall prevalance
plot(mrsa_sim_preterm, y = "i.num", qnts = 1, mean.col = "steelblue", qnts.col = "steelblue", main = "Total Prevalence")

#plot vent specific prevalance
plot(mrsa_sim_preterm, y = "i.num.pretermyes", qnts = 1, mean.col = "steelblue", qnts.col = "steelblue", main = "Disease Prevalence by Vent")
plot(mrsa_sim_preterm, y = c("i.num.pretermno", "i.num.pretermyes"),  leg = TRUE, qnts = 1, main = "Disease Prevalence by Vent")


### STATS FOR PAPER ###

nrow(NICU)
mean(census, na.rm=T); sd(census, na.rm=T)
median(NICU$LOS, na.rm=T); IQR(NICU$LOS, na.rm=T)
mean_deg_overall

#avg contacts per hr
contacts = NA
for (i in 1:nrow(NICU))
{
  admitDate = ifelse(NICU$Date_admission[i]<as.Date("2015-01-01"), as.Date("2015-01-01"), NICU$Date_admission[i])
  dischargeDate = ifelse(NICU$Date_discharge_initial[i]>as.Date("2015-05-31"), as.Date("2015-05-31"), NICU$Date_discharge_initial[i])
  contacts = c(contacts, sum(pt_interactions$ID==NICU$ID[i])/((dischargeDate-admitDate+1)*24))
}
rm(i,admitDate,dischargeDate)
mean(contacts, na.rm=T)

#table: sensitivity analysis
write.table(round(results$prevalence,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round(results$prevalence/results$denominator*100,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(paste(round(results$prev_low/results$denominator*100,1),", ",round(results$prev_high/results$denominator*100,1), sep=""), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round((results$prevalence[results$colonized==1 & results$effectivness!=0.00]-results$prevalence[results$colonized==1 & results$effectivness==0.00])/results$prevalence[results$colonized==1 & results$effectivness==0.00]*100,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round((results$prevalence[results$colonized==2 & results$effectivness!=0.00]-results$prevalence[results$colonized==2 & results$effectivness==0.00])/results$prevalence[results$colonized==2 & results$effectivness==0.00]*100,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round((results$prevalence[results$colonized==3 & results$effectivness!=0.00]-results$prevalence[results$colonized==3 & results$effectivness==0.00])/results$prevalence[results$colonized==3 & results$effectivness==0.00]*100,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round((results$prevalence[results$colonized==4 & results$effectivness!=0.00]-results$prevalence[results$colonized==4 & results$effectivness==0.00])/results$prevalence[results$colonized==4 & results$effectivness==0.00]*100,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))

#table: attributes
#preterm
mrsa_sim_mean = as.data.frame(mrsa_sim_preterm, out="mean")
mrsa_sim_sd = as.data.frame(mrsa_sim_preterm, out="sd")
conf_int_low = mrsa_sim_mean$i.num.pretermno[network_steps] - 1.96*(mrsa_sim_sd$i.num.pretermno[network_steps]/sqrt(n_sims))
conf_int_high = mrsa_sim_mean$i.num.pretermno[network_steps] + 1.96*(mrsa_sim_sd$i.num.pretermno[network_steps]/sqrt(n_sims))
results_attr = data.frame("attribute"="no","count"=mrsa_sim_mean$i.num.pretermno[network_steps],"denominator"=mrsa_sim_mean$num.pretermno[network_steps],"prevalence"=mrsa_sim_mean$i.num.pretermno[network_steps]/mrsa_sim_mean$num.pretermno[network_steps]*100,"prev_low"=conf_int_low,"prev_high"=conf_int_high,stringsAsFactors=F)
conf_int_low = mrsa_sim_mean$i.num.pretermyes[network_steps] - 1.96*(mrsa_sim_sd$i.num.pretermyes[network_steps]/sqrt(n_sims))
conf_int_high = mrsa_sim_mean$i.num.pretermyes[network_steps] + 1.96*(mrsa_sim_sd$i.num.pretermyes[network_steps]/sqrt(n_sims))
results_attr = rbind(results_attr, data.frame("attribute"="yes","count"=mrsa_sim_mean$i.num.pretermyes[network_steps],"denominator"=mrsa_sim_mean$num.pretermyes[network_steps],"prevalence"=mrsa_sim_mean$i.num.pretermyes[network_steps]/mrsa_sim_mean$num.pretermyes[network_steps]*100,"prev_low"=conf_int_low,"prev_high"=conf_int_high,stringsAsFactors=F))
rm(mrsa_sim_mean,mrsa_sim_sd,conf_int_low,conf_int_high)

write.table(round(results_attr$count,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round(results_attr$prevalence,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(paste(round(results_attr$prev_low/results_attr$denominator*100,1),", ",round(results_attr$prev_high/results_attr$denominator*100,1), sep=""), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round((results_attr$prevalence[results_attr$attribute=="yes"]-results_attr$prevalence[results_attr$attribute=="no"])/results_attr$prevalence[results_attr$attribute=="no"]*100,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))

#vent
mrsa_sim_mean = as.data.frame(mrsa_sim_vent, out="mean")
mrsa_sim_sd = as.data.frame(mrsa_sim_vent, out="sd")
conf_int_low = mrsa_sim_mean$i.num.ventno[network_steps] - 1.96*(mrsa_sim_sd$i.num.ventno[network_steps]/sqrt(n_sims))
conf_int_high = mrsa_sim_mean$i.num.ventno[network_steps] + 1.96*(mrsa_sim_sd$i.num.ventno[network_steps]/sqrt(n_sims))
results_attr = data.frame("attribute"="no","count"=mrsa_sim_mean$i.num.ventno[network_steps],"denominator"=mrsa_sim_mean$num.ventno[network_steps],"prevalence"=mrsa_sim_mean$i.num.ventno[network_steps]/mrsa_sim_mean$num.ventno[network_steps]*100,"prev_low"=conf_int_low,"prev_high"=conf_int_high,stringsAsFactors=F)
conf_int_low = mrsa_sim_mean$i.num.ventyes[network_steps] - 1.96*(mrsa_sim_sd$i.num.ventyes[network_steps]/sqrt(n_sims))
conf_int_high = mrsa_sim_mean$i.num.ventyes[network_steps] + 1.96*(mrsa_sim_sd$i.num.ventyes[network_steps]/sqrt(n_sims))
results_attr = rbind(results_attr, data.frame("attribute"="yes","count"=mrsa_sim_mean$i.num.ventyes[network_steps],"denominator"=mrsa_sim_mean$num.ventyes[network_steps],"prevalence"=mrsa_sim_mean$i.num.ventyes[network_steps]/mrsa_sim_mean$num.ventyes[network_steps]*100,"prev_low"=conf_int_low,"prev_high"=conf_int_high,stringsAsFactors=F))
rm(mrsa_sim_mean,mrsa_sim_sd,conf_int_low,conf_int_high)

write.table(round(results_attr$count,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round(results_attr$prevalence,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(paste(round(results_attr$prev_low/results_attr$denominator*100,1),", ",round(results_attr$prev_high/results_attr$denominator*100,1), sep=""), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round((results_attr$prevalence[results_attr$attribute=="yes"]-results_attr$prevalence[results_attr$attribute=="no"])/results_attr$prevalence[results_attr$attribute=="no"]*100,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))

#location
mrsa_sim_mean = as.data.frame(mrsa_sim_location, out="mean")
mrsa_sim_sd = as.data.frame(mrsa_sim_location, out="sd")
conf_int_low = mrsa_sim_mean$i.num.locationfronthall[network_steps] - 1.96*(mrsa_sim_sd$i.num.locationfronthall[network_steps]/sqrt(n_sims))
conf_int_high = mrsa_sim_mean$i.num.locationfronthall[network_steps] + 1.96*(mrsa_sim_sd$i.num.locationfronthall[network_steps]/sqrt(n_sims))
results_attr = data.frame("attribute"="fronthall","count"=mrsa_sim_mean$i.num.locationfronthall[network_steps],"denominator"=mrsa_sim_mean$num.locationfronthall[network_steps],"prevalence"=mrsa_sim_mean$i.num.locationfronthall[network_steps]/mrsa_sim_mean$num.locationfronthall[network_steps]*100,"prev_low"=conf_int_low,"prev_high"=conf_int_high,stringsAsFactors=F)
conf_int_low = mrsa_sim_mean$i.num.locationbackhall[network_steps] - 1.96*(mrsa_sim_sd$i.num.locationbackhall[network_steps]/sqrt(n_sims))
conf_int_high = mrsa_sim_mean$i.num.locationbackhall[network_steps] + 1.96*(mrsa_sim_sd$i.num.locationbackhall[network_steps]/sqrt(n_sims))
results_attr = rbind(results_attr, data.frame("attribute"="backhall","count"=mrsa_sim_mean$i.num.locationbackhall[network_steps],"denominator"=mrsa_sim_mean$num.locationbackhall[network_steps],"prevalence"=mrsa_sim_mean$i.num.locationbackhall[network_steps]/mrsa_sim_mean$num.locationbackhall[network_steps]*100,"prev_low"=conf_int_low,"prev_high"=conf_int_high,stringsAsFactors=F))
conf_int_low = mrsa_sim_mean$i.num.locationquietrm[network_steps] - 1.96*(mrsa_sim_sd$i.num.locationquietrm[network_steps]/sqrt(n_sims))
conf_int_high = mrsa_sim_mean$i.num.locationquietrm[network_steps] + 1.96*(mrsa_sim_sd$i.num.locationquietrm[network_steps]/sqrt(n_sims))
results_attr = rbind(results_attr, data.frame("attribute"="quietrm","count"=mrsa_sim_mean$i.num.locationquietrm[network_steps],"denominator"=mrsa_sim_mean$num.locationquietrm[network_steps],"prevalence"=mrsa_sim_mean$i.num.locationquietrm[network_steps]/mrsa_sim_mean$num.locationquietrm[network_steps]*100,"prev_low"=conf_int_low,"prev_high"=conf_int_high,stringsAsFactors=F))
rm(mrsa_sim_mean,mrsa_sim_sd,conf_int_low,conf_int_high)

write.table(round(results_attr$count,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round(results_attr$prevalence,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(paste(round(results_attr$prev_low/results_attr$denominator*100,1),", ",round(results_attr$prev_high/results_attr$denominator*100,1), sep=""), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))
write.table(round((results_attr$prevalence[results_attr$attribute!="fronthall"]-results_attr$prevalence[results_attr$attribute=="fronthall"])/results_attr$prevalence[results_attr$attribute=="fronthall"]*100,1), file=pipe("pbcopy"), row.names=F)
close(pipe("pbcopy"))

#top 10 interactions
round(prop.table(table(pt_interactions$Description))*100,2)[order(round(prop.table(table(pt_interactions$Description))*100,2))]


### PLOTS FOR PAPER ###

#output to tif for publication 
#tiff("Figure1.tif",height=6,width=10,units='in',res=1200) 
#tiff("Figure2_supplement.tif",height=6,width=10,units='in',res=1200) 

#plot infections
barcolors = rev(c(gray.colors(length(hygiene_eff)-1),"#FFFFFF"))
bars = barplot(matrix(data=round(results$prevalence/results$denominator*100,1) , nrow=length(hygiene_eff), ncol=length(n_colon)), col=barcolors, beside=T, ylab="Prevalence of MRSA colonization, %", xlab="No. initially colonized (prevalence, %)", names.arg=paste(n_colon," (",round(n_colon/network_size*100,1),")",sep=""), ylim=c(0,40))
margins = par()$mar
par(xpd=T, mar=c(1,2,1,2)) 
legend(1, 40, paste(hygiene_eff*100,"%",sep=""), cex=0.9, fill=barcolors, title="% Effective", x.intersp=1.5, y.intersp=1.2)
par(xpd=F, mar=margins)

#add bars
bars = as.vector(bars)
for (i in 1:length(bars))
{
  arrows(x0=bars[i], y0=(results$prev_low[i]/results$denominator[i]*100), x1=bars[i], y1=(results$prev_high[i]/results$denominator[i]*100), angle=90, length=0.06, code=3)
}
rm(i)

#close file 
#dev.off() 


#plot specific time points

#output to tif for publication 
#tiff("Figure2a.tif",height=6,width=10,units='in',res=1200) 
#tiff("Figure2b.tif",height=6,width=10,units='in',res=1200) 
#tiff("Figure2c.tif",height=6,width=10,units='in',res=1200) 

margins = par()$mar
par(xpd=T, mar=c(1,2,1,2)) 

#control vertex colors: note this only works on a given simulation, not the mean of all simulations, therefore picking simulations the represent the mean for # infected
#plot(mrsa_sim_preterm, type="network", col.status = TRUE, at=1, sims="mean")
#plot(mrsa_sim_preterm, type="network", col.status = TRUE, at=network_steps/2, sims="mean")
#plot(mrsa_sim_preterm, type="network", col.status = TRUE, at=network_steps, sims="mean")
plot(get_network(mrsa_sim_preterm, sim=13, at=1, collapse=T), vertex.col=ifelse(get.vertex.attribute(get_network(mrsa_sim_preterm, sim=13, at=1, collapse=T),"testatus")=="i","black","white"))
plot(get_network(mrsa_sim_preterm, sim=20, at=network_steps/2, collapse=T), vertex.col=ifelse(get.vertex.attribute(get_network(mrsa_sim_preterm, sim=20, at=network_steps/2, collapse=T),"testatus")=="i","black","white"))
plot(get_network(mrsa_sim_preterm, sim=40, at=network_steps, collapse=T), vertex.col=ifelse(get.vertex.attribute(get_network(mrsa_sim_preterm, sim=40, at=network_steps, collapse=T),"testatus")=="i","black","white"))

par(xpd=F, mar=margins)

#close file 
#dev.off() 


#plot prevalence curve for LOS
#tiff("Figure3_supplement.tif",height=6,width=10,units='in',res=1200) 

plot(mrsa_sim_los, leg=F, mean.col=c("#AAAAAA","#000000"), mean.lwd=3, qnts.col=c("#DDDDDD","#DDDDDD"))
legend(1500,0.6,c("Susceptible","Colonized"),lty=c(1,1),col=c("#AAAAAA","#000000"),lwd=3)

#close file 
#dev.off() 


### NODE SPECIFIC STATS ###

#get network for a given simulation, note: all simulations are same since node placement happens in netest
nodedata = get_network(mrsa_sim_location, sim=1)

#pull out individual nodes
nodedata = nodedata[[3]]

#go through each node to find vertex attributes at start
vertex_fronthall_vent = 0
vertex_fronthall_preterm = 0
vertex_backhall_vent = 0
vertex_backhall_preterm = 0
vertex_quietrm_vent = 0
vertex_quietrm_preterm = 0

for (i in 1:length(nodedata))
{
  #for this node
  node = nodedata[[i]]
  
  #only tally for nodes active at start
  if (node$active[1,1]==1)
  {
    #tally stats by location
    vertex_fronthall_vent = ifelse(node$location=="fronthall" & node$vent=="yes", vertex_fronthall_vent+1, vertex_fronthall_vent)
    vertex_fronthall_preterm = ifelse(node$location=="fronthall" & node$preterm=="yes", vertex_fronthall_preterm+1, vertex_fronthall_preterm)
    vertex_backhall_vent = ifelse(node$location=="backhall" & node$vent=="yes", vertex_backhall_vent+1, vertex_backhall_vent)
    vertex_backhall_preterm = ifelse(node$location=="backhall" & node$preterm=="yes", vertex_backhall_preterm+1, vertex_backhall_preterm)
    vertex_quietrm_vent = ifelse(node$location=="quietrm" & node$vent=="yes", vertex_quietrm_vent+1, vertex_quietrm_vent)
    vertex_quietrm_preterm = ifelse(node$location=="quietrm" & node$preterm=="yes", vertex_quietrm_preterm+1, vertex_quietrm_preterm)
  }
}
rm(i,nodedata)


### MOVIE FOR PAPER ###

library("ndtv")

## Extract the network dynamic object
nw = get_network(mrsa_sim_preterm)

## Add a time-varying color attribute to infection status
nw = color_tea(nw, verbose = FALSE)

## Animation options
slice.par = list(start = 1, end = network_steps, interval = 1,
                  aggregate.dur = 1, rule = "any")
render.par = list(tween.frames = 10, show.time = FALSE)
plot.par = list(mar = c(0, 0, 0, 0))

## Compute the animation positions of nodes
compute.animation(nw, slice.par = slice.par)

## Render the movie in a HTML5 file
render.d3movie(
  nw,
  render.par = render.par,
  plot.par = plot.par,
  vertex.cex = 0.9,
  vertex.col = "ndtvcol",
  edge.col = "darkgrey",
  vertex.border = "lightgrey",
  displaylabels = FALSE,
  filename = paste0("NICU simulation.html"))

## Render the movie in an MP4 file , requires: http://ffmpeg.org
saveVideo(
  render.animation(
    nw,
    render.par = render.par,
    plot.par = plot.par,
    vertex.cex = 0.9,
    vertex.col = "ndtvcol",
    edge.col = "darkgrey",
    vertex.border = "lightgrey",
    displaylabels = FALSE,
    render.cache = "none"),
  video.name = paste0("NICU simulation.mp4"))
