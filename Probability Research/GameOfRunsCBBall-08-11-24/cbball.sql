-- CREATE TABLE g1 (
--     game_id	INTEGER,
-- 		date TEXT,
-- 		home TEXT,
-- 		away TEXT,
-- 		play_id INTEGER,
-- 		half INTEGER,
-- 		time_remaining_half TEXT,
-- 		secs_remaining INTEGER,
-- 		secs_remaining_absolute	INTEGER,
-- 		description	TEXT,
-- 		action_team	TEXT,
-- 		home_score INTEGER,
-- 		away_score INTEGER,
-- 		score_diff INTEGER,
-- 		play_length	INTEGER,
-- 		scoring_play TEXT,
-- 		foul TEXT,
-- 		win_prob REAL,
-- 		naive_win_prob REAL,
-- 		home_time_out_remaining	INTEGER,
-- 		away_time_out_remaining	INTEGER,
-- 		home_favored_by	INTEGER,
-- 		total_line INTEGER,
-- 		referees TEXT,
-- 		arena_location TEXT,
-- 		arena TEXT,
-- 		capacity INTEGER,
-- 		attendance INTEGER,
-- 		shot_x TEXT,
-- 		shot_y TEXT,
-- 		shot_team TEXT,
-- 		shot_outcome TEXT,
-- 		shooter	TEXT,
-- 		assist	TEXT,
-- 		three_pt TEXT,
-- 		free_throw TEXT,
-- 		possession_before TEXT,
-- 		possession_after TEXT,
-- 		wrong_time TEXT
-- );

-- -- .open /cbball_data/03-01-23/401481155 g1
-- .import '/Users/erichegonzales/Projects/Wolfram/Probability Research/GameOfRunsCBBall-08-11-24/cbball.csv' 
-- g1;

-- .import /cbball_data/03-01-23/401481155.csv g1



 SELECT play_id, description, home_score, away_score FROM g13 LIMIT 10


