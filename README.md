# Clutch NBA players (2014-2015 season)

#### Project description:
Determining the top clutch NBA players using logistic regression.

#### File descriptions:
- clutch_players.R: the R code used to clean the data and perform the logistic regression
- shot_logs.csv: the uncleaned data set, sourced from Kaggle
- top50.xlsx: the cleaned data set, consisting of the top 50 NBA players

#### Introduction:
 The National Basketball Association (NBA) was founded in 1946 and has profoundly impacted our modern day society. Each season brings along more excitement and exhilarating plays as we hear the “oohhs and aahhs” coming from the crowd. We have seen players shoot countless numbers of shots in game defining moments that have drastically altered the outcomes of the game. These moments have led analysts and fans to constantly debate on which NBA player is the most “clutch.” The word “clutch” comes with many meanings, but in sports it is defined as “the phenomenon [when] athletes under pressure, usually in the last minutes of a game, summon strength, concentration and whatever else necessary to succeed, to perform well, and perhaps change the outcome of the game”. In the NBA, you either have or don’t have this clutch factor. It is used as a determinant to define how a player performs when the outcome of a game is on the line. Thus for our project, we have taken the shot data from the 2014-2015 NBA season in order to find out which player was the most clutch during that season.
 
#### Project Purpose:
 The goal of our project was to find out which NBA player had the greatest chance of making a clutch shot during the 2014-2015 season. When starting the project, we wanted to address the questions of who should have the ball in the last minutes of the game, and how exactly we would define “clutch”. The outcome of this project is designed to help coaches decide on which players should shoot the ball during clutch moments of a game. In doing so, the probabilities of winning the game will be higher.

#### Hypothesis and Reasoning:
 For our hypothesis, we choose these qualified players that we believed were going to be the most “clutch” in the NBA. The players we chose were Steph Curry, LeBron James, and Tim Duncan. Here are the reasons why we choose each player.
 
###### Steph Curry:
During the 2014-2015 season, Steph Curry won the Most Valuable Player Award.  He led the Warriors to win a franchise-record 67 games during the season, and the Warriors would go on to win the NBA championship that season. He averaged 23.8 points, 7.7 assists, 2.04 steals and 4.3 rebounds. He also broke his own NBA record by scoring 286 three-pointers. His shot percentages were elite as he shot 48.7% from the field, 44.3% from the three-point range, and a 91.4% from the free throw line. Based on an amazing FGM (field goal made) stat line we hypothesized that he makes more “clutch” shots.
 
###### LeBron James:
Standing at 6’8 tall and weighing 250 lbs of solid muscle, LeBron James has received the nickname “King James” for a reason. During the 2014-2015 season he finished third in the most valuable player race and averaged 25.3 points, 7.4 assists, and shot 53.5% from the field. With his athleticism we believed that he was able to reduce both the shot distance and closet defender distance variables. Due to his commanding presence on the court, we hypothesized that he shot many of the last minute and game-winning shots. Thus, we believe that he has higher probability of making these “clutch” shots.
 
###### Tim Duncan:
Tim Duncan is undeniably one of the best players to ever play the game of basketball. As a future hall of famer, has his a history of being called a “clutch” player. He has been knowingly to be a highly productive player in the last 5 minutes of each game. Standing at almost 7 feet tall we hypothesized that many of his shots were  two point field goals, as it would be more likely for him to score points close to the basket with ease. With closer shots we believed the variables of Touch_Time and Shot_Dist would be low. These lower variables would result in him taking higher probability shots. Thus, increasing the overall probability of him making the “clutch” shots. 


#### Analysis Process:
The shot dataset we used was extracted from Kaggle. The data consisted of 21 columns and 128,070 rows. Our first task was to clean the data. We first removed any attributes that were not relevant to our project, such as the variable MATCHUP, which contained the date the game took place. After removing these irrelevant attributes, our original 21 columns was down to 13. Our dependent variable was the FGM variable, which contained the value 1 for a made shot, or 0 for a missed shot. Our independent variables were: FINAL_MARGIN (the difference in the final game score), PERIOD (which quarter of the game), GAME_CLOCK (time in the quarter when the shot took place), SHOT_CLOCK (time on clock when shot took place), DRIBBLES (number of dribbles), TOUCH_TIME (length of time player touched ball), SHOT_DIST (how far away the shot was taken from the basket), PTS_TYPE (a two or three pointer), CLOSEST_DEFENDER (who was defending), CLOSE_DEF_DIST (distance that the defender was from the player), and player_name (name of the player who took the shot). With the FGM variable being our Y, it was moved to the 1st column position in R. 

The next step in the data cleaning process was determining what our clutch criteria would be. After doing some research, we found that clutch time was most often referred to as the last 5 minutes of a game where the score differential is 5 points or less. We then used R to create a conditional data set based on these criteria: the final margin of the game was 5 or less, the period was the 4th quarter, and the game clock was 7 minutes or greater (the last 5 minutes of a game). When dealing with the time variable of the game clock, we found that we could not create a condition simply based on the time format (minutes:seconds), so we had to change the values to numeric values. We found that the numeric value that equaled 7 minutes was 487, so our condition was that the game clock had to be greater than or equal to 487, which would ensure that the shot was taken within the last 5 minutes of the game. Our data set was now down to 2,709 rows and 13 columns. We also made some minor touch ups to the data set, as we found that some player names were misspelled (example: the player Monta Ellis was spelled Mnta Ellis). 

This cleaned data set contained the shot information for 271 players. However, this data set was a mix of both well-known starting players and lesser-known bench players. In clutch situations, coaches are more likely to put their starting players in the game. Based on this, we chose to select the top 50 players from that season, and determine who was the most clutch within those 50. We used various sources to compile a list of these top 50 players. Our data set now had to be further whittled down to only include shots taken by these top 50 players. Our final usable data set contained 644 rows, 13 columns, and 50 players. 

We first ran a logistic regression on the data set. Initially we were going to run it on everything (using the ~.), however we realized some variables were not important (such as the defender’s name and the number of points that were made from the shot). Our logistic regression contained indicator variables for every player, which is how we determined who was most clutch. The most significant variables were SHOT_CLOCK, SHOT_DIST, CLOSE_DEF_DIST, TOUCH_TIME, and DRIBBLES. It makes sense for these variables to have an effect on whether or not a shot it made. 

#### Results:
For the players, we first looked at the coefficient estimate. The higher the number, the more likely the player would make the shot. So a positive number was a good indication of a clutch player, and a negative number usually meant that the player was not very clutch. When going through these estimates, we found that the number for Jeff Teague was unusually high compared to all the other players. We looked back at our cleaned data set, and found that Jeff Teague had only taken one shot in clutch time, and he had made that shot. This meant he had a 100% success rate when taking a shot in clutch time. This was not beneficial to our models, so we decided that a player has to take at least 10 shots during clutch time. This removed any outliers that would mess with our models. 

We then looked at the P value for each player, and noticed that the coefficient estimate was not what we should be basing our decisions off of. The P value was important because it would tell us the significance of that player and the impact they would have on the game as a result. We ordered our top 50 players by P values, the lowest value being the most clutch player. From this, Andre Drummond had the lowest P value at 0.003, with Eric Bledsoe and Kyle Lowry placing in 2nd and 3rd (see Appendix A). We wanted to make sure that our results would be consistent, so we did sensitivity analysis. We took the players who made at least 20 shot attempts in clutch time, and compared those results to the 10 shot attempt players. Surprisingly, Andre Drummond was not in these new results. Eric Bledsoe and Kyle Lowry were, with Damian Lillard taking 3rd place (Lillard was also in the 10 shot attempt results, at 5th place, see Appendix A).    

#### Other models:
In excel we calculated the number of shots taken by the top 50 NBA players of that season. We recorded the players who took a shot in the last 5 minutes of the 4th quarter. We then took the number of shots taken by each player and calculated the percent of shots made which gave us our final result in excel. The best player from the top 50 NBA players of that season in clutch time was Tim Duncan with an 86% shot accuracy followed by Rudy Gay with 66.6% and Victor Oladipo with 61.5% (see Appendix A). This raw, naive model would show something interesting which will be brought up in our conclusion.  

While examining the shot data, more questions came up such as: what is the average distance a defender was from a player? What was the average time left on the shot clock? To answer these questions, we used Kernel Density Estimation (KDE). KDE’s plot the density of a specified variable, or how often a certain item occurred (see Appendix B). Our first KDE was plotting the shot distance. We found that most shots in clutch time were taken about 4-5 feet from the basket, which are shots just outside the “paint”, or “key” on the court. The other distance that was most common was around 25 feet from the basket, which is just outside the NBA three point line (23.75 feet). Both of these results make sense, as clutch shots are often made either at the basket, or from way downtown. Our second KDE was for defender distance. We found that the most common distance was 2-4 feet from the player taking the shot. This result makes sense, as a defender wants to be close enough to be able to block a shot, but not too close to where they commit a foul. 

Our third and fourth KDE were both time based: time left on the shot clock and touch time on the ball. For the shot clock, the most common time was at around 10 seconds left on the clock. This result is expected, as players do not necessarily want to run the clock down to 0, as that adds more pressure and has a higher chance of the shot being missed. For touch time (the total amount of time the player physically touched or held the ball) the common result was about 1 second. This result is also expected, as players on the court tend to do a significant amount of passing and dribbling, which reduces the time the ball is being touched.   

#### Conclusion:
As the NBA became a central element of basketball, the importance of who is the best clutch player in the NBA arose. Players such as LeBron James and Steph Curry are foreseen as the best.  However, it is crucial for the team to find a clutch player that can take them to the next level. We decided to address the question and find out who was the most clutch. The goal of our project was to find out who was clutch when it can to the last 5 minutes of the 4th quarter. We extracted our data from Kaggle with all the NBA shots taken from that season. We continued to analyze all players and clean the data to get our final 50 players. From these 50 players we were able to analyze them more closely by cleaning our data further down by removing any outliers we had. We then looked at the P value of these players to find the significance they had. We finally did a sensitivity analysis to find out the end result of who was actually the most clutch of that season. Eric Bledsoe was the most clutch followed by Kyle Lowry and Damian Lillard. We did notice that the shooting percentages for these top three were relatively low, at 28.5%, 24% and 23% respectively. However, this is not surprising, as all three of these players are guards, who tend to have lower shooting percentage than forwards and centers.  In the future, we would like to find who is the most clutch by position, the most clutch team, and who the best defender is. We would also have liked to dive into interaction variables to see how variables can influence each other. 



















