import requests
name=str(input("Enter place:"))
url = f"https://api.ambeedata.com/latest/by-city?city={name}"
headers = {
    "x-api-key": "3de0b3998988a16cbb1fbb9fdbcbeee0cc8e47d325c74c985ff14860247fbcfe"
}

response = requests.get(url, headers=headers)
data = response.json()
print(data)