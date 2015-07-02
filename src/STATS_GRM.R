# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 1989, 2014
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

# Author: Alex Reutter, IBM SPSS
# Version 1.1.2
# History
# 21-Dec-2011 fix failure to create dataset on repeated use
#
helptext="The STATS GRM command requires the R Integration Plug-in
and the R ltm package.

STATS GRM 
  /VARIABLES ITEMS=variable list
  [/OPTIONS [MISSING={LISTWISE**  }]  [EXECUTE={TRUE**}] ]
                     {ALLAVAILABLE}            {FALSE }
  [/PRINT [SUMMARY]]
  [/PLOT [FACTORSCORES] [ICC] [IIC]]
  [/SAVE [PROGRAMFILE=filespec]
 		 [FACTORSCORESDATASET=datasetname]]

Split files and weight are not honored by this command.

STATS GRM /HELP prints this information and does nothing else.

Example:
STATS GRM 
	/VARIABLES ITEMS=item1 item2 item3 item4 item5.

Executes the tpm function from the R ltm package.
The variable list specifies the items.  It is assumed that
the values of these variables is 0,1.

/OPTIONS MISSING=LISTWISE causes listwise deletion of missing values.  ALLAVAILABLE
causes all available information to be used; i.e., cases with missing values
are still used.

EXECUTE=FALSE runs the command syntax without calling the R function. 
This is mainly useful in combination with SAVE PROGRAMFILE.

/PRINT SUMMARY displays the Akaike Information Criterion (AIC) and Bayesian Information
Criterion (BIC) for the model.

/PLOT FACTORSCORES displays a plot of kernel density estimates for the factor scores 
(people parameters).

ICC displays a plot of the item characteristic curves for the model.

IIC displays a plot of the iterm information curves for the model.

/SAVE PROGRAMFILE causes the R code that implements the procedure to be 
written to the specified file. Since the R function has features not 
exposed in this extension command, the generated program can be a useful 
starting point for additional specifications.

FACTORSCORESDATASET creates a new dataset that contains a case for each observed 
response pattern, variables that describe the response patterns (one for each
item), and variables containing the Observed frequency, Expected frequency, Factor 
scores, Factor scores standard errors, and Residuals.
"

run_grm<-function(items, missing="listwise", print_summary=FALSE, 
		   plot_factorscores=FALSE, plot_icc=FALSE, plot_iic=FALSE, factorscoresdataset=NULL)
{
    domain<-"STATS_GRM"
	setuplocalization(domain)
    
    tryCatch(library(ltm), error=function(e){
        stop(gettextf("The R %s package is required but could not be loaded.","ltm",domain=domain),call.=FALSE)
        }
    )

    if (identical(missing,"listwise")) {missing<-na.exclude} else {missing<-NULL}
    dta<-spssdata.GetDataFromSPSS(items,missingValueToNA=TRUE)

    res <- tryCatch(
            grm(data=dta,na.action=missing),
            error=function(e) {return(c(gettext("ERROR:",domain=domain),e))}
           )
      
    if (!is.null(res$message)) {print(res$message)} else {
        miss<-ifelse(identical(missing,na.exclude),"na.exclude","NULL")
		nItems<-length(res$coefficients)
		
		# Start sending output to Viewer
        spsspkg.StartProcedure(gettext("Graded Respose Model",domain=domain))

		# Summary table
		if ( print_summary ) {
			information_criteria <- matrix( c(summary(res)$AIC,summary(res)$BIC), ncol=1 )
			dimnames(information_criteria)[[1]] <- c("Akaike Information Criterion (AIC)","Bayesian Information Criterion (BIC)")
			dimnames(information_criteria)[[2]] <- c("Value")
			spsspivottable.Display(information_criteria, 
				title=gettext("Summary",domain=domain),
				templateName="STATSGRM",
				isSplit=FALSE)
		}
		
		# Coefficients table
        coeff <- coefficients(res)
		dimnames(coeff)[[2]] <- c(paste("Difficulty",seq(1,length(dimnames(coeff)[[2]])-1)), "Discrimination")
		spsspivottable.Display(coeff, 
			title=gettext("Coefficients",domain=domain),
			templateName="STATSGRM",
			isSplit=FALSE)


		# Factor scores plot
		if ( plot_factorscores ) {
			plot(factor.scores(res))
		}

		# Item characteristic curves plot
		if ( plot_icc ) {
			for (i in 1:nItems){
				plot(res, type="ICC", items=i)
			}
		}

		# Item information curves plot
		if ( plot_iic ) {
			plot(res, type="IIC")
		}

		spsspkg.EndProcedure()

		# Save factor scores and residuals to new dataset
        if (!is.null(factorscoresdataset)){
            factorscoresdict = spssdictionary.CreateSPSSDictionary(c("pattern", gettext("Response pattern",domain=domain), 0, "F4.0", "ordinal"))
            for (i in 1:nItems){
                factorscoresdict<-spssdictionary.CreateSPSSDictionary(factorscoresdict, c(dimnames(coefficients(res))[[1]][i], gettextf("Item %s",i,domain=domain), 0, "F4.0", "scale"))
            }
			factorscoresdict<-spssdictionary.CreateSPSSDictionary(factorscoresdict, 
															   c("obs", gettextf("Observed frequency",domain=domain), 0, "F4.0", "scale"),
															   c("exp", gettextf("Expected frequency",domain=domain), 0, "F8.2", "scale"),
															   c("fscores", gettextf("Factor scores",domain=domain), 0, "F8.2", "scale"),
															   c("fscores_se", gettextf("Factor scores standard errors",domain=domain), 0, "F8.2", "scale"),
															   c("residuals", gettextf("Residuals",domain=domain), 0, "F8.2", "scale")
															   )
            tryCatch({
                spssdictionary.SetDictionaryToSPSS(factorscoresdataset, factorscoresdict)
                factorscoresdata <- list()
				factorscoresdata[[1]] <- seq(1,length(factor.scores(res)$score.dat[,1]))
                for (i in 2:(nItems+1)) {factorscoresdata[[i]] <- factor.scores(res)$score.dat[,(i-1)]}
				factorscoresdata[[nItems+2]] <- factor.scores(res)$score.dat["Obs"][,1]
				factorscoresdata[[nItems+3]] <- factor.scores(res)$score.dat["Exp"][,1]
				factorscoresdata[[nItems+4]] <- factor.scores(res)$score.dat["z1"][,1]
				factorscoresdata[[nItems+5]] <- factor.scores(res)$score.dat["se.z1"][,1]
				resid <- residuals(res)
				for (i in 1:nItems) { resid <- resid[order(resid[,(nItems-i+1)]),] }
				resid <- resid[,(nItems+3)]
				resid[resid=="Inf"] <- NaN
				factorscoresdata[[nItems+6]] <- resid
				
				
                factorscoresdata = data.frame(factorscoresdata)
                spssdata.SetDataToSPSS(factorscoresdataset, factorscoresdata)
				spssdictionary.EndDataStep()   # 12/21/11
                }, 
                error=function(e) {print(e)
                cat(gettext("Failed to create factor scores dataset. Dataset name must not already exist: ",domain=domain),factorscoresdataset)
                }
            )
        }

	}

    res <- tryCatch(rm(list=ls()),warning=function(e){return(NULL)})
    
}

caller<-function(items, missing="listwise", programfile=NULL, execute="true", print_summary=FALSE, 
		   plot_factorscores=FALSE, plot_icc=FALSE, plot_iic=FALSE, factorscoresdataset=NULL){
    
    if(!is.null(programfile)){
        title<-"# STATS GRM\n"

		if ( !is.null(factorscoresdataset) ) {
            pfit <- paste("factorscoresdataset<-",dQuote(factorscoresdataset),sep="")
		} else {
            pfit <- "factorscoresdataset<-NULL"			
		}
        lines<-c(title,
            "run_grm<-",
            attr(run_grm,"source"),
            paste("items<-",deparse(items),sep=""),
            paste("missing<-",dQuote(missing),sep=""),
            paste("print_summary<-",print_summary,sep=""),
            paste("plot_factorscores<-",plot_factorscores,sep=""),
            paste("plot_icc<-",plot_icc,sep=""),
            paste("plot_iic<-",plot_iic,sep=""),
			pfit,
			"run_grm(items,missing,print_summary,plot_factorscores,plot_icc,plot_iic,factorscoresdataset)")
        f<-file(description=programfile,open="wb",encoding="UTF-8")
        writeLines(lines,con=f)
        close(f)
    }
    
    if (execute=="true") run_grm(items,missing,print_summary,plot_factorscores,plot_icc,plot_iic,factorscoresdataset)
    
}

setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 

Run<-function(args){
    
    cmdname = args[[1]]
    args <- args[[2]]
    oobj<-spsspkg.Syntax(templ=list(
                spsspkg.Template("ITEMS", subc="VARIABLES",  ktype="existingvarlist", var="items", islist=TRUE),
                spsspkg.Template("MISSING", subc="OPTIONS",ktype="str", var="missing"),
                spsspkg.Template("EXECUTE", subc="OPTIONS", ktype="str", var="execute"),
                spsspkg.Template("SUMMARY", subc="PRINT", ktype="bool", var="print_summary"),
                spsspkg.Template("FACTORSCORES", subc="PLOT", ktype="bool", var="plot_factorscores"),
                spsspkg.Template("ICC", subc="PLOT", ktype="bool", var="plot_icc"),
                spsspkg.Template("IIC", subc="PLOT", ktype="bool", var="plot_iic"),
                spsspkg.Template("PROGRAMFILE", subc="SAVE", ktype="literal", var="programfile"),
                spsspkg.Template("FACTORSCORESDATASET", subc="SAVE", ktype="literal", var="factorscoresdataset")
                ))

    if ("HELP" %in% attr(args,"names"))
        #writeLines(helptext)
        helper(cmdname)
    else
        res <- spsspkg.processcmd(oobj,args,"caller")
        
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}