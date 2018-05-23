# Shopping cart

Create a simple shopping cart application with the following features:

- Add a product into a shopping cart: idProduct + quantity
- Print the ticket of the shopping cart to the console, taking into account the offers

Consider 2 products:

- Chocolate bar (idProduct: 1)
- Baguette (idProduct: 2)

Consider 1 offer:

- 3 baguettes: 20% off

## Acceptance criteria

Ticket should have the following the format:

```
10/01/2018 14:00:00

PRODUCTS:

 Chocolate bar       2.00

 Baguette            0.80

TOTAL: 2.80
```

```
10/01/2018 14:00:00

PRODUCTS:

 Chocolate bar       4.00
 2 x 2.00

 Baguette            0.80

TOTAL: 4.80
```

```
10/01/2018 14:00:00

PRODUCTS:

 Chocolate bar       2.00

 Baguette            4.00
 5 x 0.80

 Baguette Discount  -0.48

TOTAL: 5.52
```

## Requirements

This class must be used:
```
public class ShoppingCartService {

  public void addItem(int productId, int quantity);

  public void printTicket();

}
```