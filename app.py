import requests
name=str(input("Enter place:"))
url = f"https://api.ambeedata.com/latest/by-city?city={name}"
headers = {
    "x-api-key": "XXXX"
}

response = requests.get(url, headers=headers)
data = response.json()
print(data)