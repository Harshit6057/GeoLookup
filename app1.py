import requests
import json
import os
from datetime import datetime

def fetch_air_quality(city):
    url = f"https://api.ambeedata.com/latest/by-city?city={city}"
    api_key = os.environ.get("AMBEE_API_KEY")
    if not api_key:
        raise RuntimeError("AMBEE_API_KEY environment variable is not configured")

    headers = {
        "x-api-key": api_key
    }

    response = requests.get(url, headers=headers)
    
    if response.status_code != 200:
        return {"City": city, "Error": f"HTTP Error {response.status_code}", "Time": datetime.now().isoformat()}
    
    data = response.json()

    if "stations" in data and len(data["stations"]) > 0:
        station = data["stations"][0]
        result = {
            "City": city,
            "Place": station.get("placeName", "Unknown"),
            "State": station.get("state", "Unknown"),
            "Country": station.get("countryCode", "Unknown"),
            "Latitude": station.get("lat"),
            "Longitude": station.get("lng"),
            "PM2.5": station.get("PM25"),
            "PM10": station.get("PM10"),
            "NO2": station.get("NO2"),
            "SO2": station.get("SO2"),
            "CO": station.get("CO"),
            "OZONE": station.get("OZONE"),
            "AQI": station.get("AQI"),
            "Main Pollutant": station.get("aqiInfo", {}).get("pollutant", "Unknown"),
            "AQI Category": station.get("aqiInfo", {}).get("category", "Unknown"),
            "Time": station.get("updatedAt", datetime.now().isoformat())
        }
        return result
    else:
        return {"City": city, "Error": "No data found", "Time": datetime.now().isoformat()}


# ----------- Run Script -----------
if __name__ == "__main__":
    city_name = input("Enter City Name: ")
    data = fetch_air_quality(city_name)

    print(json.dumps(data, indent=2))

    # Overwrite same file every time
    with open("latest_air_quality.json", "w") as f:
        json.dump(data, f, indent=2)

    print("\n✅ JSON file overwritten: latest_air_quality.json")
