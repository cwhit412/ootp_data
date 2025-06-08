#example import script

#load packages
#have to install first
install.packages('tidyverse')
#after first installation, remove that line and use this one instead
library('tidyverse')

#first, read the file in

bronze <- read.csv('file location')

#innings are in .1, .2 etc format: gotta update

bronze$IP <- as.integer(bronze$IP) + (bronze$IP%% 1 * (10/3))

#then have to prep some calculations:

##first: wOBA weights. from tom tango blog post
##calculate league totals, and then create weights from that

#calculating amount of various outcomes

bronzetotals <- bronze %>%
  summarise(PAsum=sum(PA),
            ABsum=sum(AB),
            singlesum=sum(X1B.1),
            doublesum=sum(X2B.1),
            triplesum=sum(X3B.1),
            HRsum=sum(HR),
            BBsum=sum(BB),
            IBBsum=sum(IBB),
            HPsum=sum(HP),
            SFsum=sum(SF),
            SHsum=sum(SH),
            SBsum=sum(SB),
            CSsum=sum(CS),
            outsum=sum(IP*3),
            runssum=sum(R),
            triplerate=sum(X3B.1)/(sum(X3B.1+X2B.1))) #close enough for outs

wobaweights_bronze <- bronzetotals %>%
  mutate(RperPA=runssum/PAsum,
         rperout=runssum/outsum,
         rbb=rperout+.14,
         rhbp=rbb+0.025,
         r1b=rbb+0.155,
         r2b=r1b+0.3,
         r3b=r2b+0.27,
         rhr=1.4,
         rsb=0.2,
         rcs=2*rperout+0.075,
         runval=r1b*singlesum+r2b*doublesum+r3b*triplesum+HRsum*rhr+(BBsum-IBBsum)*rbb+rhbp*HPsum+rsb*SBsum-rcs*CSsum,
         unproouts=ABsum-(singlesum+doublesum+triplesum+HRsum)+SFsum,
         proouts=(BBsum-IBBsum+HPsum+singlesum + doublesum +triplesum + HRsum),
         runminus=runval/unproouts,
         runplus=runval/proouts,
         wobanoscale=proouts/(ABsum+BBsum-IBBsum+HPsum+SFsum),
         wobascale=1/(runminus+runplus),
         wobabb = (rbb+runminus)*wobascale,
         wobaHBP = (rhbp+runminus)*wobascale,
         woba1B = (r1b+runminus)*wobascale,
         woba2B = (r2b+runminus)*wobascale,
         woba3B = (r3b+runminus)*wobascale,
         wobaHR = (rhr+runminus)*wobascale,
         wobaSB = (rsb*wobascale),
         wobaCS = (rcs*wobascale))

##then FIP constant

bronzetotalpitch <- bronze %>%
  summarize(IP = sum(IP),
            ERA = round(9* sum(ER) / sum(IP),2),
            FIP = round(((13*sum(HR.1))+(3*(sum(BB.1)+sum(HP.1)))-(2*sum(K)))/sum(IP),2),
            K9 = round(9*sum(K)/(sum(IP)),2),
            BB9 = round(9*sum(BB.1)/(sum(IP)),2),
            KBB = round(sum(K)/sum(BB.1),2),
            HR9 = round(9*sum(HR.1)/(sum(IP)),2),
            BABIP = round((sum(X1B.2+X2B.2+X3B.2+HR.1) - sum(HR.1))/(sum(AB.1) - sum(HR.1) - sum(K) + sum(SF.1)),3))



bronze$fipconstant <- bronzetotalpitch$ERA - bronzetotalpitch$FIP

#now can summarize things
##hitting first
###can calculate more stats, here are an example of some.
###need to group by name, CID, and VLvl to make sure that different cards aren't counted together
#positions included so you can filter by positions

bronzehit <- bronze %>%
  group_by(Name,CID,VLvl) %>%
  summarize(PA = sum(PA), 
            AB = sum(AB),
            B = paste(unique(B)),
            BA = sum(H) / sum(AB),
            OBP = (sum(H) + sum(BB) + sum(HP))/(sum(AB) + sum(BB) + sum(HP) + sum(SF)),
            SLG = sum(X1B.1*1+X2B.1*2+X3B.1*3+HR*4) / sum(AB),
            BBper = round((sum(BB) / sum(PA)*100),2),
            SOper = round((sum(SO) / sum(PA)*100),2),
            HRper600 = (sum(HR)/sum(PA))*600,
            BABIP = round((sum(X1B.1+X2B.1+X3B.1+HR) - sum(HR))/(sum(AB) - sum(HR) - sum(SO) + sum(SF)),3),
            WARper600 =round(sum(WAR) / (sum(PA)/600),2),
            wOBA = (wobaweights_bronze$wobabb*(sum(BB)-sum(IBB)) + wobaweights_bronze$wobaHBP*sum(HP) + wobaweights_bronze$woba1B*sum(X1B.1) + wobaweights_bronze$woba2B*sum(X2B.1) + wobaweights_bronze$woba3B*sum(X3B.1) + wobaweights_bronze$wobaHR*sum(HR))/(sum(AB)+sum(BB)-sum(IBB)+sum(SF)+ sum(HP)),
            SBper600 = round(sum(SB) / (sum(PA)/600),2),
            CSper600 = round(sum(CS) / (sum(PA)/600),2),
            SBpct = sum(SB) /(sum(SB) + sum(CS)),
            BatRper600 = round(sum(BatR) / (sum(PA)/600),2),
            sbRper600 = round(sum(wSB) / (sum(PA)/600),2),
            UBRper600 = round(sum(UBR) / (sum(PA)/600),2),
            ZRper150 = round((sum(ZR)/sum(IP.1))*(150*9),2),
            tripper = (sum(X3B.1))/(sum(X2B.1 + X3B.1)),
            xbhper = (sum(X2B.1) + sum(X3B.1))/(sum(X1B.1 + X2B.1 + X3B.1)),
            firstdef =round(mean(as.numeric(X1B.Pot))),
            seconddef =round(mean(as.numeric(X2B.Pot))),
            thirddef =round(mean(as.numeric(X3B.Pot))),
            ssdef =round(mean(as.numeric(SS.Pot))),
            lfdef =round(mean(as.numeric(LF.Pot))),
            rfdef =round(mean(as.numeric(RF.Pot))),
            cfdef =round(mean(as.numeric(CF.Pot))),
            catchdef =round(mean(as.numeric(C.Pot)))
  ) %>%
  mutate(OPS = OBP + SLG) 

#view the results: can change the PA number to restrict to whatever samples size you want. 
##can also do more restrictions if you learn how
View(bronzehit[bronzehit$PA>500,])


###pitching summary

bronzepitch <- bronze%>%
  group_by(Name,CID,VLvl) %>%
  summarize(IP = sum(IP),
            GSper = sum(GS.1)/sum(G.1),
            IPpergame = sum(IP)/sum(G.1),
            T = paste(unique(T)),
            RA9 = round(9* sum(R.1) / sum(IP),2),
            ERA = round(9* sum(ER) / sum(IP),2),
            FIP = round(((13*sum(HR.1))+(3*(sum(BB.1)+sum(HP.1)))-(2*sum(K)))/sum(IP) + unique(bronze$fipconstant),2),
            wOBA = (wobaweights_bronze$wobabb*(sum(BB.1)-sum(IBB.1)) + wobaweights_bronze$wobaHBP*sum(HP.1) + wobaweights_bronze$woba1B*sum(X1B.2) + wobaweights_bronze$woba2B*sum(X2B.2) + wobaweights_bronze$woba3B*sum(X3B.2) + wobaweights_bronze$wobaHR*sum(HR.1))/(sum(AB.1)+sum(BB.1)-sum(IBB.1)+sum(SF.1)+ sum(HP.1)),
            K9 = round(9*sum(K)/(sum(IP)),2),
            BB9 = round(9*sum(BB.1)/(sum(IP)),2),
            HR9 = round(9*sum(HR.1)/(sum(IP)),2),
            BABIP = round((sum(X1B.2+X2B.2+X3B.2+HR.1) - sum(HR.1))/(sum(AB.1) - sum(HR.1) - sum(K) + sum(SF.1)),3))

View(bronzepitch[bronzepitch$IP>300,])
