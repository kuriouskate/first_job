SELECT
	A.Customer AS Name,
	RIGHT(A.ID, 12) as ID,
	A.Store,
	SUM(A.O_PRICE-A.Ch_PRICE) as SPEND,
	SUM(Quantity) as ITEMS,
 	MIN(B.first_date) AS first_visit,
	MAX(B.last_date) AS recent_visit
FROM
	[Table].[dbo].[Cashier] A
LEFT OUTER JOIN [Table].[dbo].[Customers] B
ON A.Customer_ID=B.Custer_ID
WHERE
	A.Date BETWEEN '7/4/2015' AND '7/5/2015'
	AND A.Kind='TX_SALE'
	AND A.ID<>'400003763856'
GROUP BY A.ID, A.Customer, A.Store
ORDER BY A.ID
