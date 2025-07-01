## Code taken from https://github.com/metacoglab/HMeta-d/blob/master/R/trials2counts.R
# MIT License
# 
# Copyright (c) 2017 Stephen Fleming
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#####################################

#Trial 2 Count function
#
# Convert trial by trial experimental information for N trials into response counts.
#
# INPUTS
# stimID:   1xN vector. stimID(i) = 0 --> stimulus on i'th trial was S1.
#                       stimID(i) = 1 --> stimulus on i'th trial was S2.
#
# response: 1xN vector. response(i) = 0 --> response on i'th trial was "S1".
#                       response(i) = 1 --> response on i'th trial was "S2".
#
# rating:   1xN vector. rating(i) = X --> rating on i'th trial was X.
#                       X must be in the range 1 <= X <= nRatings.
#
# nRatings: total # of available subjective ratings available for the
#           subject. e.g. if subject can rate confidence on a scale of 1-4,
#           then nRatings = 4
#
# padAmount: if set to 1, each response count in the output has the value of
#          1/(2*nRatings) added to it. This is desirable if trial counts of
#          0 interfere with model fitting.
#          if set to 0, trial counts are not manipulated and 0s may be
#          present. default value is 0.
# padCells: Putts padAmount into function. default value is 0.
#
# OUTPUTS
# newlist which contains nR_S1 & nR_S2
# these are vectors containing the total number of responses in
# each response category, conditional on presentation of S1 and S2.
#
# e.g. if nR_S1 = [100 50 20 10 5 1], then when stimulus S1 was
# presented, the subject had the following response counts:
# responded S1, rating=3 : 100 times
# responded S1, rating=2 : 50 times
# responded S1, rating=1 : 20 times
# responded S2, rating=1 : 10 times
# responded S2, rating=2 : 5 times
# responded S2, rating=3 : 1 time
#
# EXAMPLE
# stimID =    list(0, 1, 0, 0, 1, 1, 1, 1)
# response =  list(0, 1, 1, 1, 0, 0, 1, 1)
# rating =    list(1, 2, 3, 4, 4, 3, 2, 1)
# nRatings = 4
# newlist = trials2counts(stimID,response,rating,nRatings)
# print(newlist)

#####################################

trials2counts <- function(stimID, response, rating,nRatings, padAmount = 0,padCells=0){

	nR_S1 <- list()
	nR_S2 <- list()

	if (padAmount == 0){
		padAmount = 1/(2*nRatings)}
	# S1 responses
	for (r in nRatings:1){
		cs1 <- 0
		cs2 <- 0
		for (i in 1:length(stimID)){
			s = stimID[i]
			x = response[i]
			y = rating[i]

			if ((s==0) & (x==0) & (y==r)){
				(cs1 <- cs1+1)}
			if ((s==1) & (x==0) & (y==r)){
				(cs2 <- cs2+1)}
		}
		nR_S1 <- append(nR_S1,cs1)
		nR_S2 <- append(nR_S2,cs2)
	}

	# S2 responses
	for (r in 1:nRatings){
		cs1 <- 0
		cs2 <- 0
		for (i in 1:length(stimID)){
			s = stimID[i]
			x = response[i]
			y = rating[i]

			if ((s==0) & (x==1) & (y==r)){
				(cs1 <- cs1+1)}
			if ((s==1) & (x==1) & (y==r)){
				(cs2 <- cs2+1)}
		}
		nR_S1 <- append(nR_S1,cs1)
		nR_S2 <- append(nR_S2,cs2)
	}


	# pad response counts to avoid zeros
	nR_S1 <- as.numeric(nR_S1)
	nR_S2 <- as.numeric(nR_S2)
	if (padCells == 1){
		nR_S1 <- lapply(nR_S1,FUN= function(x) x+padAmount)
		nR_S2 <- lapply(nR_S2,FUN= function(x) x+padAmount)}

	# Combine into lists
	newlist <- list(nR_S1,nR_S2)
}
