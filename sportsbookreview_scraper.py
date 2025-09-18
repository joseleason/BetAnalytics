import requests
import json
import pandas as pd
import numpy as np
from bs4 import BeautifulSoup
from datetime import datetime, timedelta

# Base URLs for different bet types
BASE_URLS = {
    "spread": "https://www.sportsbookreview.com/betting-odds/nba-basketball/?date=",
    "moneyline": "https://www.sportsbookreview.com/betting-odds/nba-basketball/money-line/full-game/?date=",
    "totals": "https://www.sportsbookreview.com/betting-odds/nba-basketball/totals/full-game/?date="
}

# Define date range
start_date = datetime(2019, 10, 22)
end_date = datetime(2025, 7, 1)
delta = timedelta(days=1)

# Container for collected data
all_games = []

# Function to fetch and parse JSON data from a given URL
def fetch_odds_data(url):
    response = requests.get(url, headers={"User-Agent": "Mozilla/5.0"})
    if response.status_code != 200:
        print(f"Failed to fetch {url}, Status Code: {response.status_code}")
        return None

    soup = BeautifulSoup(response.text, "html.parser")
    script_tag = soup.find("script", id="__NEXT_DATA__")

    if not script_tag:
        print(f"No JSON data found for {url}")
        return None

    try:
        json_data = json.loads(script_tag.string)
        return json_data["props"]["pageProps"]["oddsTables"][0]["oddsTableModel"]["gameRows"]
    except Exception as e:
        print(f"Error parsing JSON for {url}: {e}")
        return None

# Loop through each date
while start_date <= end_date:
    formatted_date = start_date.strftime("%Y/%m/%d")
    print(f"Scraping data for {formatted_date}")

    # Fetch data for each bet type
    spread_data = fetch_odds_data(BASE_URLS["spread"] + formatted_date)
    moneyline_data = fetch_odds_data(BASE_URLS["moneyline"] + formatted_date)
    totals_data = fetch_odds_data(BASE_URLS["totals"] + formatted_date)

    # Convert to dictionaries for easy merging
    spread_dict = {game["gameView"]["gameId"]: game for game in spread_data} if spread_data else {}
    moneyline_dict = {game["gameView"]["gameId"]: game for game in moneyline_data} if moneyline_data else {}
    totals_dict = {game["gameView"]["gameId"]: game for game in totals_data} if totals_data else {}

    # Process all games from spread data (since it's the main dataset)
    for game_id, game_info in spread_dict.items():
        game_view = game_info["gameView"]

        away_team = game_view["awayTeam"]["fullName"]
        away_team_abbr = game_view["awayTeam"]["shortName"]
        away_team_score = game_view["awayTeamScore"]

        home_team = game_view["homeTeam"]["fullName"]
        home_team_abbr = game_view[ "homeTeam" ][ "shortName" ]
        home_team_score = game_view[ "homeTeamScore" ]

        game_city = game_view["city"]
        game_state = game_view["state"]
        game_country = game_view["country"]

        game_date = game_view["startDate"][:10]  # Keep only the date

        # Function to compute the median odds from multiple sportsbooks
        def get_median_odds(odds_data, key, line, bet_type = 'spread'):
            filtered_values = [
                o[line][key] for o in odds_data # and o["sportsbook"] in ["bet365","betmgm", "caesars"]
                if o and o[line].get(key) is not None and (bet_type == "moneyline" or (
                        bet_type == "spread" and -115 <= o[ line ].get('homeOdds') <= -105 and -115 <= o[ line ].get(
                    'awayOdds') <= -105) or (
                                                                                                                         bet_type == "total" and -115 <= o[ line ].get('overOdds') <= -105 and -115 <=
                                                                                                                         o[
                                                                                                                             line ].get(
                                                                                                                             'underOdds') <= -105))
            ]
            return np.median(filtered_values) if filtered_values else None

        # Extract median spread odds
        spread_odds = [o for o in game_info["oddsViews"] if o]

        spread_median_open = get_median_odds(spread_odds, "homeSpread", 'openingLine', 'spread')
        home_spread_ml_median_open = get_median_odds(spread_odds, "homeOdds", 'openingLine', 'spread')
        away_spread_ml_median_open = get_median_odds(spread_odds, "awayOdds", 'openingLine', 'spread')

        spread_median = get_median_odds(spread_odds, "homeSpread", 'currentLine', 'spread')
        home_spread_ml_median = get_median_odds(spread_odds, "homeOdds", 'currentLine', 'spread')
        away_spread_ml_median = get_median_odds(spread_odds, "awayOdds", 'currentLine', 'spread')

        # Extract median moneyline odds
        moneyline_odds = moneyline_dict.get(game_id, {}).get("oddsViews", [])
        home_moneyline_median_open = get_median_odds(moneyline_odds, "homeOdds",'openingLine', "moneyline")
        away_moneyline_median_open = get_median_odds(moneyline_odds, "awayOdds",'openingLine', "moneyline")

        home_moneyline_median = get_median_odds(moneyline_odds, "homeOdds", 'currentLine', "moneyline")
        away_moneyline_median = get_median_odds(moneyline_odds, "awayOdds", 'currentLine', "moneyline")

        # Extract median totals odds
        totals_odds = totals_dict.get(game_id, {}).get("oddsViews", [])
        totals_median_open = get_median_odds(totals_odds, "total",'openingLine', "total")
        over_ml_median_open = get_median_odds(totals_odds, "overOdds",'openingLine', "total")
        under_ml_median_open = get_median_odds(totals_odds, "underOdds",'openingLine', "total")

        totals_median = get_median_odds(totals_odds, "total", 'currentLine', "total")
        over_ml_median = get_median_odds(totals_odds, "overOdds", 'currentLine', "total")
        under_ml_median = get_median_odds(totals_odds, "underOdds", 'currentLine', "total")

        # Store the data
        all_games.append({
            "GameId": game_id,
            "GameDate": game_date,
            "GameCity": game_city,
            "GameState": game_state,
            "GameCountry": game_country,
            "AwayTeam": away_team,
            "AwayTeamAbbr": away_team_abbr,
            "AwayTeamScore": away_team_score,
            "HomeTeam": home_team,
            "HomeTeamAbbr": home_team_abbr,
            "HomeTeamScore": home_team_score,
            "CloseSpread": spread_median,
            "CloseHomeSpreadMoneyline": home_spread_ml_median,
            "CloseAwaySpreadMoneyline": away_spread_ml_median,
            "CloseHomeMoneyline": home_moneyline_median,
            "CloseAwayMoneyline": away_moneyline_median,
            "CloseTotals": totals_median,
            "CloseOverMoneyline": over_ml_median,
            "CloseUnderMoneyline": under_ml_median,
            "OpenSpread": spread_median_open,
            "OpenHomeSpreadMoneyline": home_spread_ml_median_open,
            "OpenAwaySpreadMoneyline": away_spread_ml_median_open,
            "OpenHomeMoneyline": home_moneyline_median_open,
            "OpenAwayMoneyline": away_moneyline_median_open,
            "OpenTotals": totals_median_open,
            "OpenOverMoneyline": over_ml_median_open,
            "OpenUnderMoneyline": under_ml_median_open,
        })

    # Move to the next date
    start_date += delta

# Convert collected data into a DataFrame and save
df = pd.DataFrame(all_games)
df.to_parquet("dl/sbr_nba_odds.parquet")
print(df)